// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: warn when the same receiver has 3+ consecutive message sends in a
//! statement sequence that could be written as a cascade.
//!
//! **DDD Context:** Compilation
//!
//! ```text
//! // Bad — three separate sends to the same receiver
//! arr add: 1.
//! arr add: 2.
//! arr add: 3.
//!
//! // Good — use cascade syntax
//! arr add: 1; add: 2; add: 3
//! ```
//!
//! Only simple receivers (local variables, `self`, class references) are
//! checked.  Complex receiver expressions (e.g. `(foo bar) baz`) are skipped
//! because they may have side effects and are not straightforward to extract
//! into a single cascade receiver.
//!
//! A run on a value receiver whose sends answer updated copies
//! (`p withX: 1. p withY: 2. p withZ: 3.`) is not suggested: the cascade it
//! would become is exactly what the `cascade-drops-update` warning flags
//! (ADR 0145 § 3). The run is skipped when any send but the last draws that
//! warning, judged by the same predicate the type checker uses
//! ([`cascade_drops_update`]) against the module's inferred receiver types.

use std::cell::OnceCell;

use crate::{LintContext, LintPass};
use beamtalk_core::ast::{
    BinarySegment, Block, Expression, ExpressionStatement, Identifier, Module,
};
use beamtalk_core::semantic_analysis::type_checker::cascade_drops_update;
use beamtalk_core::semantic_analysis::{ClassHierarchy, TypeMap, infer_types};
use beamtalk_core::source_analysis::Diagnostic;

/// Lint pass that warns on 3+ consecutive message sends to the same simple
/// receiver that could be rewritten as a cascade.
pub(crate) struct CascadeCandidatePass;

impl LintPass for CascadeCandidatePass {
    fn check(&self, module: &Module, ctx: &LintContext<'_>, diagnostics: &mut Vec<Diagnostic>) {
        let types = Types {
            module,
            hierarchy: ctx.hierarchy,
            map: OnceCell::new(),
        };
        // Skip module.expressions — top-level sequences are used in bootstrap-test
        // files as individual test assertions (each paired with its own `// =>`
        // comment). Converting those to a cascade would change the semantics
        // because only the last send's result would be asserted. Only check
        // method bodies and standalone method definitions.
        for class in &module.classes {
            // Skip TestCase subclasses — test methods naturally have many
            // consecutive sends to the same actor/object for setup and
            // assertion, and cascading would change return semantics.
            if class
                .superclass
                .as_ref()
                .is_some_and(|s| s.name == "TestCase")
            {
                continue;
            }
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                check_sequence(&method.body, &types, diagnostics);
            }
        }
        for standalone in &module.method_definitions {
            check_sequence(&standalone.method.body, &types, diagnostics);
        }
    }
}

/// The module's inferred types, computed on first use: only a candidate run
/// needs them, so a module with none never pays for type inference.
struct Types<'a> {
    module: &'a Module,
    hierarchy: &'a ClassHierarchy,
    map: OnceCell<TypeMap>,
}

impl Types<'_> {
    /// Whether turning `run` (sends to one simple receiver) into a cascade
    /// would draw the `cascade-drops-update` warning: some send but the last
    /// answers an updated copy of a value receiver.
    fn cascade_would_drop_update(&self, run: &[ExpressionStatement]) -> bool {
        let Some((_last, non_final)) = run.split_last() else {
            return false;
        };
        non_final.iter().any(|stmt| {
            let Expression::MessageSend {
                receiver, selector, ..
            } = &stmt.expression
            else {
                return false;
            };
            let map = self
                .map
                .get_or_init(|| infer_types(self.module, self.hierarchy, None));
            map.get(receiver.span())
                .is_some_and(|ty| cascade_drops_update(self.hierarchy, ty, &selector.name()))
        })
    }
}

/// Returns the simple receiver name if `expr` is a synchronous `MessageSend`
/// whose receiver is an identifier (variable, `self`) or a class reference.
///
/// Cast sends (`is_cast: true`) are excluded because `CascadeMessage` does not
/// support cast semantics — a cast cannot appear in a cascade.
fn simple_receiver_name(expr: &Expression) -> Option<&str> {
    if let Expression::MessageSend {
        receiver, is_cast, ..
    } = expr
    {
        if *is_cast {
            return None;
        }
        match receiver.as_ref() {
            Expression::Identifier(Identifier { name, .. }) => Some(name.as_str()),
            Expression::ClassReference { name, .. } => Some(name.name.as_str()),
            _ => None,
        }
    } else {
        None
    }
}

/// Check a flat sequence of statements for runs of 3+ consecutive sends to
/// the same simple receiver, then recurse into nested expressions.
fn check_sequence(
    exprs: &[ExpressionStatement],
    types: &Types<'_>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut i = 0;
    while i < exprs.len() {
        if let Some(recv) = simple_receiver_name(&exprs[i].expression) {
            let run_start = i;
            while i < exprs.len() && simple_receiver_name(&exprs[i].expression) == Some(recv) {
                i += 1;
            }
            let run_len = i - run_start;
            // Skip `self` — consecutive sends to `self` in method bodies are
            // typically independent assertions or setup steps (e.g. BUnit's
            // `self assert:…`) that should remain as separate statements for
            // readability and debuggability.  Cascading them would merge
            // logically-distinct operations and make test failures harder to
            // isolate.
            if run_len >= 3
                && recv != "self"
                && !types.cascade_would_drop_update(&exprs[run_start..i])
            {
                let first_span = exprs[run_start].expression.span();
                let last_span = exprs[i - 1].expression.span();
                let span = first_span.merge(last_span);
                diagnostics.push(
                    Diagnostic::lint(
                        format!(
                            "`{recv}` receives {run_len} consecutive messages; consider using a cascade"
                        ),
                        span,
                    )
                    .with_hint(format!(
                        "Replace the {run_len} separate sends with a cascade: \
                         `{recv} msg1; msg2; ...`"
                    )),
                );
            }
            // Recurse into each expression in the run
            for stmt in &exprs[run_start..i] {
                recurse(&stmt.expression, types, diagnostics);
            }
        } else {
            recurse(&exprs[i].expression, types, diagnostics);
            i += 1;
        }
    }
}

/// Recurse into nested statement sequences (block bodies, etc.) that may
/// themselves contain cascade candidates.
fn recurse(expr: &Expression, types: &Types<'_>, diagnostics: &mut Vec<Diagnostic>) {
    match expr {
        Expression::Block(Block { body, .. }) => {
            check_sequence(body, types, diagnostics);
        }

        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            recurse(receiver, types, diagnostics);
            for arg in arguments {
                recurse(arg, types, diagnostics);
            }
        }

        Expression::Assignment { target, value, .. } => {
            recurse(target, types, diagnostics);
            recurse(value, types, diagnostics);
        }

        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            recurse(value, types, diagnostics);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            recurse(receiver, types, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    recurse(arg, types, diagnostics);
                }
            }
        }

        Expression::Parenthesized { expression, .. } => {
            recurse(expression, types, diagnostics);
        }

        Expression::FieldAccess { receiver, .. } => {
            recurse(receiver, types, diagnostics);
        }

        Expression::Match { value, arms, .. } => {
            recurse(value, types, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    recurse(guard, types, diagnostics);
                }
                recurse(&arm.body, types, diagnostics);
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                recurse(&pair.key, types, diagnostics);
                recurse(&pair.value, types, diagnostics);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                recurse(elem, types, diagnostics);
            }
            if let Some(t) = tail {
                recurse(t, types, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } | Expression::TupleLiteral { elements, .. } => {
            for elem in elements {
                recurse(elem, types, diagnostics);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let beamtalk_core::ast::StringSegment::Interpolation(e) = seg {
                    recurse(e, types, diagnostics);
                }
            }
        }
        Expression::BinaryConstruction { segments, .. } => {
            for e in segments.iter().flat_map(BinarySegment::sub_expressions) {
                recurse(e, types, diagnostics);
            }
        }

        // Leaf nodes — nothing to recurse into.
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Error { .. }
        | Expression::Spread { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::run_lint_passes;
    use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        run_lint_passes(&module)
    }

    #[test]
    fn three_consecutive_sends_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  add: arr =>\n    arr add: 1\n    arr add: 2\n    arr add: 3\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "expected one lint diagnostic, got: {diags:?}"
        );
        assert_eq!(diags[0].severity, Severity::Lint);
        assert!(
            diags[0].message.contains("cascade"),
            "message: {}",
            diags[0].message
        );
    }

    #[test]
    fn four_consecutive_sends_flagged_once() {
        let diags = lint(
            "Object subclass: Foo\n  add: arr =>\n    arr add: 1\n    arr add: 2\n    arr add: 3\n    arr add: 4\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "expected one diagnostic for the whole run, got: {diags:?}"
        );
        assert!(
            diags[0].message.contains('4'),
            "message should mention run length: {}",
            diags[0].message
        );
    }

    #[test]
    fn two_consecutive_sends_not_flagged() {
        let diags = lint("Object subclass: Foo\n  add: arr =>\n    arr add: 1\n    arr add: 2\n");
        assert!(
            diags.is_empty(),
            "two sends should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn interleaved_sends_not_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  add: arr with: other =>\n    arr add: 1\n    other add: 99\n    arr add: 2\n    arr add: 3\n",
        );
        assert!(
            diags.is_empty(),
            "non-consecutive sends should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn lint_has_category_and_hint() {
        let diags = lint(
            "Object subclass: Foo\n  add: arr =>\n    arr add: 1\n    arr add: 2\n    arr add: 3\n",
        );
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
        assert!(diags[0].hint.is_some(), "expected a hint");
    }

    #[test]
    fn self_receiver_not_flagged() {
        // Consecutive sends to `self` (e.g. BUnit assertions) are intentionally
        // exempt from the cascade lint — they represent independent operations
        // that should remain as separate statements for debuggability.
        let diags =
            lint("Object subclass: Foo\n  run =>\n    self doA\n    self doB\n    self doC\n");
        assert!(
            diags.is_empty(),
            "self sends should NOT be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn complex_receiver_not_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  run: x =>\n    (x builder) add: 1\n    (x builder) add: 2\n    (x builder) add: 3\n",
        );
        assert!(
            diags.is_empty(),
            "complex receivers should not be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn cascade_in_block_body_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  run: arr =>\n    [:x | arr add: 1. arr add: 2. arr add: 3]\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "consecutive sends in block body should be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn mixed_selector_types_flagged() {
        // Unary + keyword sends to the same receiver are still a cascade candidate.
        let diags = lint(
            "Object subclass: Foo\n  run: arr =>\n    arr add: 1\n    arr sort\n    arr add: 2\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "mixed selector types should still be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn cast_send_breaks_run() {
        // Cast sends (`!`) cannot appear in a cascade, so they break the run.
        let diags = lint(
            "Object subclass: Foo\n  run: arr =>\n    arr add: 1\n    arr fire!\n    arr add: 2\n",
        );
        assert!(
            diags.is_empty(),
            "cast send should break the run, got: {diags:?}"
        );
    }

    #[test]
    fn expect_directive_breaks_run() {
        // `@expect` directives are separate expression nodes that break runs.
        // This is correct: an @expect-annotated send has different semantics.
        let diags = lint(
            "Object subclass: Foo\n  run: arr =>\n    arr add: 1\n    @expect dnu\n    arr add: 2\n    arr add: 3\n",
        );
        assert!(
            diags.is_empty(),
            "@expect should break the run, got: {diags:?}"
        );
    }

    #[test]
    fn test_case_subclass_not_flagged() {
        // TestCase subclasses have many consecutive sends to actors for setup —
        // these should not trigger the cascade lint.
        let diags = lint(
            "TestCase subclass: MyTest\n  testSomething =>\n    actor setX: 1\n    actor setY: 2\n    actor start\n",
        );
        assert!(
            diags.is_empty(),
            "TestCase methods should NOT be flagged, got: {diags:?}"
        );
    }

    #[test]
    fn class_method_body_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  class\n  setup: arr =>\n    arr add: 1\n    arr add: 2\n    arr add: 3\n",
        );
        assert_eq!(
            diags.len(),
            1,
            "class method body should be checked, got: {diags:?}"
        );
    }

    #[test]
    fn value_run_that_would_drop_updates_not_flagged() {
        // BT-4512 (ADR 0145 § 3): as a cascade, `p withX: 1; withY: 2;
        // withZ: 3` would draw `cascade-drops-update`, so the lint must not
        // suggest it.
        let diags = lint(
            "Value subclass: Point\n  field: x :: Integer = 0\n  field: y :: Integer = 0\n  field: z :: Integer = 0\n\nObject subclass: Foo\n  run: p :: Point =>\n    p withX: 1\n    p withY: 2\n    p withZ: 3\n",
        );
        assert!(
            diags.is_empty(),
            "a run the cascade-drops-update warning would flag should NOT be suggested, got: {diags:?}"
        );
    }

    #[test]
    fn dictionary_run_that_would_drop_updates_not_flagged() {
        let diags = lint(
            "Object subclass: Foo\n  run: d :: Dictionary =>\n    d at: #a put: 1\n    d at: #b put: 2\n    d at: #c put: 3\n",
        );
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    #[test]
    fn value_run_answering_other_types_still_flagged() {
        // Sends that do not answer an updated copy (`printString`, `x`) cascade
        // safely, so the suggestion stands.
        let diags = lint(
            "Value subclass: Point\n  field: x :: Integer = 0\n\nObject subclass: Foo\n  run: p :: Point =>\n    p printString\n    p x\n    p withX: 3\n",
        );
        assert_eq!(diags.len(), 1, "got: {diags:?}");
    }
}
