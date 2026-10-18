// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0133 Phase 3 (BT-3909): `beamtalk-core`'s `group_by_fold_threads`, the
//! rule the `GroupByValue` residual check reads, agrees with the fold gate
//! codegen actually takes for a `groupBy:` (`block_needs_mutation_threading`:
//! `needs_mutation_threading`, the nested list-op cross-scope scan and the
//! REPL bindings scan, over the block profile). Over a corpus of block bodies
//! that write no local bound outside the block (those are the Phase 0 check's),
//! in an actor instance method and at the REPL, codegen lowers the threaded
//! fold (a `GroupMap` accumulator) exactly when the core rule says so. A body
//! codegen threads that the rule misses would answer the raw carrier tuple
//! without a diagnostic; one the rule names that codegen does not is an
//! over-rejection.

use crate::core_erlang::tests::codegen;
use crate::core_erlang::{CodeGenContext, CoreErlangGenerator};
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::{RootMethodKind, group_by_fold_threads};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

/// Statements of a `groupBy:` block over `x` (`f` is an outer callable, `n` an
/// actor field; `self` sends and field writes are actor-only).
const STATEMENTS: &[(&str, bool)] = &[
    ("x", false),
    ("k := x > 2", false),
    ("#(_a, _b) := #(x, 0)", false),
    ("y := 1. y := y + 1", false),
    ("true ifTrue: [v := x]", false),
    ("nil ifNil: [v := x] ifNotNil: [:w | w]", false),
    ("x > 0 and: [v := x. true]", false),
    ("#(1) do: [:e | w := e]", false),
    ("u := 0. #(1) do: [:e | u := e]", false),
    (
        "#(1) do: [:e | w := 0. [w < 2] whileTrue: [w := w + 1]]",
        false,
    ),
    ("true ifTrue: [#(1) do: [:e | z := e]]", false),
    ("(#(1) collect: [:e | e]) size", false),
    ("#(3) inject: 0 into: [:a :e | a + e]", false),
    ("#(3) anySatisfy: f", false),
    ("[:q | q] value: x", false),
    ("[w := x] value", false),
    ("[w := x] on: Error do: [:e | 0]", false),
    ("1 to: 2 do: [:i | w := i]", false),
    ("self.n := x", true),
    ("self plain", true),
    ("self.n", true),
];

/// The `groupBy:` block literal of the first statement of `body`.
fn group_by_block(body: &[beamtalk_core::ast::ExpressionStatement]) -> &beamtalk_core::ast::Block {
    let Expression::MessageSend { arguments, .. } = &body[0].expression else {
        panic!("not a send");
    };
    let Some(Expression::Block(block)) = arguments.first() else {
        panic!("no block argument");
    };
    block
}

#[test]
fn group_by_fold_gate_agreement() {
    let mut bodies: Vec<(String, bool)> = STATEMENTS
        .iter()
        .map(|(s, actor_only)| ((*s).to_string(), *actor_only))
        .collect();
    for (a, a_actor) in STATEMENTS {
        for (b, b_actor) in STATEMENTS.iter().step_by(3) {
            bodies.push((format!("{a}. {b}"), *a_actor || *b_actor));
        }
    }
    let mut threaded = [0usize; 2];
    for (stmts, actor_only) in &bodies {
        let send = format!("items groupBy: [:x | {stmts}. x > 2]");
        // An actor instance method.
        let src = format!(
            "Actor subclass: Srv\n  state: n = 0\n\n  plain => 0\n\n  run: items with: f =>\n    {send}\n"
        );
        let (module, _) = parse(lex_with_eof(&src));
        let block = group_by_block(&module.classes[0].methods[1].body);
        let rule = group_by_fold_threads(block, Some(RootMethodKind::Actor));
        let code = codegen(&src);
        let fold = code.contains("GroupMap");
        assert_eq!(
            fold, rule,
            "actor `{send}`: codegen fold {fold}, core rule {rule}\n{code}"
        );
        threaded[0] += usize::from(fold);
        if *actor_only {
            continue;
        }
        // The REPL, with `items` and `f` as workspace bindings.
        let (module, _) = parse(lex_with_eof(&format!("{send}\n")));
        let block = group_by_block(&module.expressions);
        let rule = group_by_fold_threads(block, None);
        let mut generator = CoreErlangGenerator::new("agree_group_by");
        generator.context = CodeGenContext::Repl;
        generator.set_is_repl_mode(true);
        generator.push_scope();
        generator.bind_var("__bindings__", "Bindings");
        let (doc, _) = generator
            .expression_doc_with_repl_mutation_tracking(&module.expressions[0].expression)
            .unwrap_or_else(|e| panic!("REPL `{send}`: {e:?}"));
        let code = doc.to_pretty_string();
        let fold = code.contains("GroupMap");
        assert_eq!(
            fold, rule,
            "REPL `{send}`: codegen fold {fold}, core rule {rule}\n{code}"
        );
        threaded[1] += usize::from(fold);
    }
    // Not vacuous: both sides take the fold for some bodies and not others.
    assert!(
        threaded[0] > 10 && threaded[0] < bodies.len(),
        "{threaded:?}"
    );
    assert!(threaded[1] > 5, "{threaded:?}");
}
