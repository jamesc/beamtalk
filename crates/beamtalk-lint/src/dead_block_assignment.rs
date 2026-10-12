// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when a block literal writes an outer local but sits where no
//! call site threads the write back, and no compile error already says so.
//!
//! **DDD Context:** Compilation
//!
//! A block that assigns a local of its enclosing method (an *outer local*,
//! ADR 0131) only has that write seen by the caller when the block is run
//! inline at a call site codegen threads (ADR 0041: loops, conditionals,
//! list ops, `on:do:`/`ensure:`, a literal block's `value`). This pass is
//! derived from the same core facts as the ADR 0131 compile errors rather
//! than a scan of its own:
//!
//! - **What a block writes** is [`block_facts::outer_local_writes`], the fact
//!   behind §6 and the Phase 0 allow-set.
//! - **Which positions thread** is the shared
//!   [`beamtalk_core::state_threading_selectors`] table
//!   (`is_state_threaded_block_arg` / `is_state_threaded_block_receiver`),
//!   the one codegen's `threaded_locals_of` reads.
//! - **Shapes a compile error rejects** are never warned about: a write
//!   inside the span of a [`local_threading_diagnostics`] error (§6
//!   `Tier2BlockNoReturnChannel`, Phase 0 `UnmigratedLocalThreading`) is
//!   that error's to report, so `beamtalk lint` and `beamtalk build` never
//!   flag the same write twice, and when a later phase widens either error
//!   this lint steps back without a change here.
//!
//! What is left for the lint (pinned by `lint_and_section6_agree` below):
//!
//! - a block stored or returned but never sent (`blk := [x := 2]`,
//!   `^[x := 2]`, `#(blk)`, a field store) — §6 errors only once the block
//!   reaches a send (BT-3756 tracks the rest);
//! - an Erlang FFI block argument, which §6 exempts for good (ADR 0041
//!   §Erlang Interop Boundary: lossy by design);
//! - a block literal at a construct position §6 accepts but the shared
//!   table does not thread (a `whileTrue:` condition, BT-3782; `eachWithIndex:` /
//!   `do:separatedBy:` outside an actor, `tryDo:` until ADR 0131 Phase 4),
//!   which crashes or loses the write at runtime today. As the Phase 0
//!   allow-set grows over these (BT-3753) the lint steps back from each.
//!
//! ```text
//! // Fine — do: threads the write back
//! count := 0
//! #(1, 2, 3) do: [:item | count := count + 1]
//!
//! // Warned — blk is stored and never sent, so its write never reaches count
//! blk := [count := count + 1]
//! ```
//!
//! Not warned at all: Actor subclasses (their blocks thread through the
//! actor's own state protocol, and §6 covers what does not), and a block's
//! own locals (`[t := 1. t := 2]`).
//!
//! [`block_facts::outer_local_writes`]: beamtalk_core::semantic_analysis::block_facts::outer_local_writes

use std::collections::HashSet;

use crate::{LintPass, hierarchy_for_lint};
use beamtalk_core::ast::{
    Block, ClassKind, Expression, ExpressionStatement, MethodDefinition, Module, StringSegment,
};
use beamtalk_core::semantic_analysis::block_facts::outer_local_writes;
use beamtalk_core::semantic_analysis::{
    extract_match_arm_bindings, extract_pattern_bindings, local_threading_diagnostics,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// Lint pass that warns about outer-local writes in blocks that no call site
/// threads back, on value types.
pub(crate) struct DeadBlockAssignmentPass;

impl LintPass for DeadBlockAssignmentPass {
    fn check(&self, module: &Module, diagnostics: &mut Vec<Diagnostic>) {
        // See `hierarchy_for_lint` doc comment for why this is needed
        // instead of `class.class_kind`.
        let hierarchy = hierarchy_for_lint(module);

        let rejected: Vec<Span> = local_threading_diagnostics(module, &hierarchy)
            .iter()
            .flat_map(|d| std::iter::once(d.span).chain(d.notes.iter().filter_map(|n| n.span)))
            .collect();
        let mut out = Out {
            rejected: &rejected,
            reported: HashSet::new(),
            diagnostics,
        };

        // Top-level expressions (script context — always value type semantics)
        let mut scope = LintScope::new();
        walk_expr_seq(&module.expressions, &mut scope, &mut out);

        for class in &module.classes {
            // Skip Actor subclasses (including indirect ones).
            if hierarchy.resolve_class_kind(&class.name.name) == ClassKind::Actor {
                continue;
            }
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                check_method(method, &mut out);
            }
        }

        // Standalone method definitions: resolve the full ancestor chain
        // rather than only the direct superclass.
        for standalone in &module.method_definitions {
            if hierarchy.resolve_class_kind(&standalone.class_name.name) == ClassKind::Actor {
                continue;
            }
            check_method(&standalone.method, &mut out);
        }
    }
}

/// Where warnings go, and what is not warned about.
struct Out<'a> {
    /// The spans of the ADR 0131 compile errors for this module and of
    /// their notes: a write is theirs when one of these contains it (each
    /// error both spans its block or construct and notes the write itself).
    rejected: &'a [Span],
    /// `(local, write span)` pairs already warned about: a write inside
    /// nested unthreaded blocks is seen once per enclosing block.
    reported: HashSet<(String, Span)>,
    diagnostics: &'a mut Vec<Diagnostic>,
}

// ── Scope tracking ────────────────────────────────────────────────────────────

/// Lightweight scope stack tracking which variables are bound at each depth.
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

    fn is_bound(&self, name: &str) -> bool {
        self.levels.iter().any(|level| level.contains(name))
    }
}

// ── Traversal ─────────────────────────────────────────────────────────────────

/// Check a method: push a new scope, define method parameters, traverse body.
fn check_method(method: &MethodDefinition, out: &mut Out<'_>) {
    let mut scope = LintScope::new();
    scope.push();
    for param in &method.parameters {
        scope.define(param.name.name.as_str());
    }
    walk_expr_seq(&method.body, &mut scope, out);
    scope.pop();
}

/// Where a block literal sits relative to the message send it belongs to.
#[derive(Debug, Clone, Copy)]
enum BlockPosition<'s> {
    /// Not a direct receiver or argument of a send: stored, returned, an
    /// element of a literal, a statement of its own, ...
    Value,
    /// The receiver of `selector` (`[...] value`, `[...] ensure: [...]`).
    Receiver(&'s str),
    /// The argument at this index of `selector`.
    Arg(&'s str, usize),
}

impl BlockPosition<'_> {
    /// Whether codegen threads a block literal's outer-local writes back to
    /// the caller at this position, per the shared
    /// [`beamtalk_core::state_threading_selectors`] table.
    fn threads(self) -> bool {
        use beamtalk_core::state_threading_selectors as table;
        match self {
            BlockPosition::Value => false,
            BlockPosition::Receiver(selector) => table::is_state_threaded_block_receiver(selector),
            BlockPosition::Arg(selector, index) => {
                table::is_state_threaded_block_arg(selector, index)
            }
        }
    }
}

/// Walk a sequence of expressions in order.
fn walk_expr_seq(exprs: &[ExpressionStatement], scope: &mut LintScope, out: &mut Out<'_>) {
    for stmt in exprs {
        walk_expr(&stmt.expression, scope, out);
    }
}

/// Recursively walk a single expression, tracking bindings and checking
/// every block literal by its position.
fn walk_expr(expr: &Expression, scope: &mut LintScope, out: &mut Out<'_>) {
    #[allow(clippy::enum_glob_use)]
    use Expression::*;

    match expr {
        Assignment { target, value, .. } => {
            walk_expr(value, scope, out);
            match target.as_ref() {
                Identifier(id) => scope.define(id.name.as_str()),
                other => walk_expr(other, scope, out),
            }
        }

        Block(block) => enter_block(block, BlockPosition::Value, scope, out),

        MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } => {
            let selector = selector.name();
            if let Block(block) = receiver.as_ref() {
                enter_block(block, BlockPosition::Receiver(&selector), scope, out);
            } else {
                walk_expr(receiver, scope, out);
            }
            walk_msg_args(&selector, arguments, scope, out);
        }

        Cascade {
            receiver, messages, ..
        } => {
            walk_expr(receiver, scope, out);
            for msg in messages {
                walk_msg_args(&msg.selector.name(), &msg.arguments, scope, out);
            }
        }

        FieldAccess { receiver, .. } => walk_expr(receiver, scope, out),
        Return { value, .. } => walk_expr(value, scope, out),
        Parenthesized { expression, .. } => walk_expr(expression, scope, out),

        DestructureAssignment { pattern, value, .. } => {
            walk_expr(value, scope, out);
            let (ids, _) = extract_pattern_bindings(pattern);
            for id in ids {
                scope.define(id.name.as_str());
            }
        }

        Match { value, arms, .. } => {
            walk_expr(value, scope, out);
            for arm in arms {
                // Pattern-bound variables are local to the arm.
                scope.push();
                let (ids, _) = extract_match_arm_bindings(&arm.pattern);
                for id in ids {
                    scope.define(id.name.as_str());
                }
                if let Some(guard) = &arm.guard {
                    walk_expr(guard, scope, out);
                }
                walk_expr(&arm.body, scope, out);
                scope.pop();
            }
        }

        MapLiteral { pairs, .. } => {
            for pair in pairs {
                walk_expr(&pair.key, scope, out);
                walk_expr(&pair.value, scope, out);
            }
        }

        ListLiteral { elements, tail, .. } => {
            for elem in elements {
                walk_expr(elem, scope, out);
            }
            if let Some(t) = tail {
                walk_expr(t, scope, out);
            }
        }

        ArrayLiteral { elements, .. } => {
            for elem in elements {
                walk_expr(elem, scope, out);
            }
        }

        StringInterpolation { segments, .. } => {
            for seg in segments {
                if let StringSegment::Interpolation(e) = seg {
                    walk_expr(e, scope, out);
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

/// Walk message arguments, checking each block literal by its position.
fn walk_msg_args(
    selector: &str,
    arguments: &[Expression],
    scope: &mut LintScope,
    out: &mut Out<'_>,
) {
    for (i, arg) in arguments.iter().enumerate() {
        if let Expression::Block(block) = arg {
            enter_block(block, BlockPosition::Arg(selector, i), scope, out);
        } else {
            walk_expr(arg, scope, out);
        }
    }
}

/// Check a block literal at `position`, then walk its body in a new scope.
fn enter_block(
    block: &Block,
    position: BlockPosition<'_>,
    scope: &mut LintScope,
    out: &mut Out<'_>,
) {
    if !position.threads() {
        for write in outer_local_writes(block, &|name| scope.is_bound(name)) {
            let rejected = out.rejected.iter().any(|r| r.contains(write.span));
            if !rejected && out.reported.insert((write.name.to_string(), write.span)) {
                emit_dead_assignment_warning(&write.name, write.span, out.diagnostics);
            }
        }
    }
    scope.push();
    for param in &block.parameters {
        scope.define(param.name.as_str());
    }
    walk_expr_seq(&block.body, scope, out);
    scope.pop();
}

/// Emit a dead-assignment warning diagnostic.
fn emit_dead_assignment_warning(name: &str, span: Span, diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.push(
        Diagnostic::lint(
            format!(
                "assignment to outer local `{name}` inside a block that no call site \
                 threads back: the block is stored, returned, or passed where its write \
                 to `{name}` is lost (or, for an escaped block, raises at runtime)"
            ),
            span,
        )
        .with_hint(
            "Pass the block literal directly to a loop, conditional, or iteration \
             method that threads it (do:, collect:, inject:into:, ifTrue:, ...), or \
             return the new value from the block and assign it, e.g. with \
             `inject:into:` to accumulate a value as the block's own result."
                .to_string(),
        )
        .with_category(DiagnosticCategory::DeadAssignment),
    );
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
    /// compiler cannot thread the mutation back out (invoking such a
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
    /// block mutations still propagate for actors, no warning.
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
    /// ancestor chain, not just the direct superclass.
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

    // ── State-threaded call sites no longer warn ────────────────────────────
    //
    // The compiler's Value-type / class-method state-threading (ADR 0041,
    // including conditionals) threads a captured-and-mutated outer
    // local back out for a block literal passed directly to any of these
    // selectors — confirmed at runtime by the existing `mutation_corpus_value.bt`
    // / `mutation_corpus_class_method.bt` / `counted_loop_mutation_test.bt` BUnit
    // corpora and by `stdlib/test/bt3385dead_assignment_test.bt`.

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
    /// too — no longer flagged.
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

    /// `and:`/`or:` thread captured-local mutations too — no longer flagged.
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

    /// The shared canonical table also covers `on:do:`'s handler block and
    /// `ensure:`'s cleanup block: codegen threads both the same way as the
    /// loop/conditional family — see `is_state_threaded_block_arg`'s doc
    /// comment — so these are no longer false positives.
    #[test]
    fn on_do_and_ensure_handler_no_longer_warn() {
        for src in [
            "x := 1.\n[nil] on: Error do: [:e | x := 2]",
            "x := 1.\n[nil] ensure: [x := 2]",
        ] {
            let diags = lint(src);
            assert!(
                diags.is_empty(),
                "Expected no lints for {src:?}, got: {diags:?}"
            );
        }
    }

    /// A block literal in RECEIVER position of a selector codegen inlines
    /// (`is_state_threaded_block_receiver`) threads its captured-local
    /// mutation just like a recognized block argument does — `[count :=
    /// count + 1] value` was the exact BT-1213 shape and has asserted
    /// `count = 1` afterwards in `block_evaluation_test.bt` ever since, and
    /// the `ensure:`/`on:do:` try bodies are the `do_accumulator_value.bt`
    /// BT-3173 shapes. Runtime pins for every selector here:
    /// `stdlib/test/dead_assignment_receiver_threading_test.bt`.
    #[test]
    fn threaded_receiver_block_no_warn() {
        for src in [
            "count := 0.\n[count := count + 1] value",
            "total := 0.\n[:n | total := total + n] value: 7",
            "total := 0.\n[:a :b | total := total + a + b] value: 1 value: 2",
            "sum := 0.\n[sum := sum + 1] ensure: [nil]",
            "sum := 0.\n[sum := sum + 1] on: Error do: [:e | nil]",
            // Nested inside a recognized loop body, the receiver rule still applies.
            "sum := 0.\n#(1, 2) do: [:each | [sum := sum + each] ensure: [nil]]",
        ] {
            let diags = lint(src);
            assert!(
                diags.is_empty(),
                "Expected no lints for {src:?}, got: {diags:?}"
            );
        }
    }

    /// A block literal at a send codegen does not inline (a user-defined
    /// selector, `perform:withArguments:`, `valueWithArguments:`) or a stored
    /// block later sent anything is an ADR 0131 §6 compile error, so the
    /// lint leaves it to that error rather than flagging the write twice.
    #[test]
    fn section6_rejected_shapes_do_not_warn() {
        for src in [
            "count := 0.\nfoo customLoop: [:item | count := count + 1]",
            "count := 0.\n[count := count + 1] customRun: 1",
            "count := 0.\n[count := count + 1] perform: #value withArguments: #()",
            "count := 0.\n[:x | count := count + x] valueWithArguments: #(1)",
            "count := 0.\nblk := [count := count + 1].\nblk value",
        ] {
            let diags = lint(src);
            assert!(
                diags.is_empty(),
                "Expected no lints for {src:?}, got: {diags:?}"
            );
        }
        // Nested: §6 rejects the inner block's write to `y`; the stored outer
        // block's own write to `x` is still the lint's.
        let diags = lint("x := 0.\ny := 0.\nblk := [x := 1. foo run: [y := 2]]");
        assert_eq!(diags.len(), 1, "Expected only `x` to warn, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    /// Construct positions §6 accepts but the shared table does not thread (BT-3782, BT-3753)
    /// still warn: in a value-type context a keyword `whileTrue:`/
    /// `whileFalse:` CONDITION block's write crashes at runtime today
    /// ("function expects 1 arguments but was called with 0"), and
    /// `eachWithIndex:`/`do:separatedBy:` lose theirs.
    #[test]
    fn unthreaded_construct_positions_still_warn() {
        for src in [
            "i := 0.\n[i := i + 1. i < 3] whileTrue",
            "i := 0.\n[i := i + 1. i < 3] whileTrue: [nil]",
            "i := 0.\n[i := i + 1. i >= 3] whileFalse: [nil]",
            "i := 0.\n#(1, 2) eachWithIndex: [:e :ix | i := i + e]",
            "i := 0.\n#(1, 2) do: [:e | nil] separatedBy: [i := i + 1]",
        ] {
            let diags = lint(src);
            assert_eq!(
                diags.len(),
                1,
                "Expected 1 lint for {src:?}, got: {diags:?}"
            );
        }
    }

    /// A threaded construct nested inside a stored block does not hide the
    /// stored block's own escape: the write is lost because `blk` is never
    /// sent, whatever runs it inline inside the block.
    #[test]
    fn threaded_construct_inside_stored_block_warns() {
        let diags = lint("x := 0.\nblk := [#(1, 2) do: [:e | x := x + e]]");
        assert_eq!(diags.len(), 1, "Expected 1 lint, got: {diags:?}");
        assert!(diags[0].message.contains("`x`"));
    }

    /// The whole family of loop / list-op selectors that codegen's
    /// `threaded_locals_of` (`crates/beamtalk-codegen/src/core_erlang/
    /// threading_analysis.rs`) recognizes for captured-local threading — none of these
    /// should warn on a mutation of an outer local at the recognized
    /// block-argument position, whatever the accumulator's name.
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
            // accumulator parameter, threads too.
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

    // ── @expect dead_assignment suppression ─────────────────────────────────

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

    // ── Agreement with the ADR 0131 compile errors ────────────────────────────

    /// Who reports an outer-local write in a shape.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum Owner {
        /// Threaded back by codegen: neither the lint nor a compile error.
        Neither,
        /// An ADR 0131 compile error (§6 or the Phase 0 allow-set); no lint.
        CompileError,
        /// The lint alone: a block value stored or returned but never sent.
        Lint,
        /// Wrong at runtime today but accepted by §6: exactly one of the two,
        /// whichever owns it at the moment (the Phase 0 allow-set is being
        /// widened over these, BT-3753; the lint steps back when it is).
        Either,
    }

    /// One shape set checked against both `beamtalk lint` and the compile
    /// errors `beamtalk build` reports (the full [`analyse`] pipeline, not
    /// the [`local_threading_diagnostics`] shortcut the lint itself uses):
    /// they never both report a shape, and each shape lands with the owner
    /// listed. Each shape is a statement of a value-type method that has
    /// outer locals `x` and `i` bound and reads `x` afterwards.
    ///
    /// [`analyse`]: beamtalk_core::semantic_analysis::analyse
    #[test]
    fn lint_and_section6_agree() {
        use Owner::{CompileError, Either, Lint, Neither};
        let shapes: &[(&str, Owner)] = &[
            // Threaded call sites.
            ("#(1, 2) do: [:e | x := x + e]", Neither),
            ("#(1, 2) inject: 0 into: [:a :e | x := x + e. a]", Neither),
            ("1 to: 2 do: [:k | x := x + k]", Neither),
            ("[i < 2] whileTrue: [i := i + 1. x := x + 1]", Neither),
            ("x > 0 ifTrue: [x := 1] ifFalse: [x := 2]", Neither),
            ("[x := x + 1] value", Neither),
            ("[x := 1] on: Error do: [:e | x := 2]", Neither),
            ("[nil] ensure: [x := 2]", Neither),
            ("blk := [:t | t := 1. t]", Neither),
            // §6: a block value that reaches a send with no return channel.
            ("self customLoop: [x := x + 1]", CompileError),
            ("[x := x + 1] customRun: 1", CompileError),
            ("[:n | x := x + n] valueWithArguments: #(1)", CompileError),
            ("self run: ([x := 1])", CompileError),
            ("self run: [x := 1]; yourself", CompileError),
            ("blk := [x := 1]\n    blk value", CompileError),
            ("blk := [x := 1]\n    self run: blk", CompileError),
            // A block stored or returned and never sent.
            ("blk := [x := 2]", Lint),
            ("blk := [x := 2]\n    #[blk]", Lint),
            ("blk := [x := 2]\n    ^blk", Lint),
            ("^[x := 2]", Lint),
            ("#[[x := 2]]", Lint),
            ("blk := [#(1, 2) do: [:e | x := x + e]]", Lint),
            // An Erlang FFI argument: lossy by design (ADR 0041 §Erlang
            // Interop Boundary), so §6 exempts it for good.
            ("(Erlang lists) map: [:e | x := x + 1. e] with: #(1)", Lint),
            // Accepted by §6, wrong at runtime today (probed in a TestCase:
            // the condition crashes, eachWithIndex:/do:separatedBy: answer 0).
            ("[i := i + 1. i < 3] whileTrue: [nil]", Either),
            ("#(1, 2) eachWithIndex: [:e :k | x := x + e]", Either),
            ("#(1, 2) do: [:e | nil] separatedBy: [x := x + 1]", Either),
            // Raises a type_error today (PIN-BUG BT-3743, ADR 0131 Phase 4).
            ("Result tryDo: [x := x + 1. 1]", Either),
        ];
        let compile_error_categories = [
            DiagnosticCategory::Tier2BlockNoReturnChannel,
            DiagnosticCategory::UnmigratedLocalThreading,
        ];
        let mut failures = Vec::new();
        for &(shape, expected) in shapes {
            let src = format!(
                "Object subclass: Probe\n  run =>\n    x := 0\n    i := 0\n    {shape}\n    x\n"
            );
            let (module, parse_diags) = parse(lex_with_eof(&src));
            assert!(
                parse_diags.iter().all(|d| d.severity != Severity::Error),
                "shape {shape:?} does not parse: {parse_diags:?}"
            );
            let compile_error = beamtalk_core::semantic_analysis::analyse(&module)
                .diagnostics
                .iter()
                .any(|d| {
                    d.category
                        .is_some_and(|c| compile_error_categories.contains(&c))
                });
            let mut lint_diags = Vec::new();
            DeadBlockAssignmentPass.check(&module, &mut lint_diags);
            let lint = !lint_diags.is_empty();
            let actual = match (compile_error, lint) {
                (false, false) => Neither,
                (true, false) => CompileError,
                (false, true) => Lint,
                (true, true) => {
                    failures.push(format!("{shape:?}: reported by both"));
                    continue;
                }
            };
            let ok = actual == expected || (expected == Either && actual != Neither);
            if !ok {
                failures.push(format!("{shape:?}: expected {expected:?}, got {actual:?}"));
            }
        }
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }
}
