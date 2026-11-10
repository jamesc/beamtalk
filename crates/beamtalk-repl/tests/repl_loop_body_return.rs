// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4288 / BT-4312: a `^` run in place at the REPL (in the body of a loop
//! or collection operation, at the root of the input, in a conditional arm,
//! an evaluated or protected block) ends the input with its value, as in a
//! method, and the workspace bindings written before it keep their values.
//!
//! Two generated corpora. The BT-4312 one ([`root_rows`]) crosses every
//! position of a `^` outside a loop with its position in a multi-statement
//! input; see [`ROOT_SHAPES`]. The BT-4288 one: every loop and fold kind
//! whose body is run in place (`beamtalk-core`'s
//! `block_facts::InPlaceReturns::in_place`), crossed with the
//! position of the `^` in the body (a statement, a conditional arm, an
//! evaluated block, a nested loop, a protected block, a `match:` arm, before a
//! binding write, an assigned value, a body that writes no binding, and one
//! that never fires), and the
//! position of the loop in the input (after a binding write, alone, before
//! another statement, as an operand, in a conditional arm). Every row is
//! interpreted by [`expected`], the reference semantics of a method, and:
//!
//! 1. compiles with the debug `ThreadedIr` verifier on and through `erlc`;
//! 2. with the REPL eval boundary switched off
//!    (`repl_nlr::boundary_fault::without_boundary`), fails with
//!    `VerifyError::ReplReturnWithoutBoundary`, the check that fails on the
//!    repro before the fix;
//! 3. runs on a BEAM against the runtime, in eval and trace mode, and answers
//!    the value and the bindings [`expected`] says.
//!
//! Both corpora hold rows whose `^` runs an `ensure:` cleanup that writes a
//! binding (BT-4312): the cleanup's write reaches the eval's boundary with the
//! throw (proof 3), and with the `ensure:` relay switched off
//! (`ensure_nlr_relay::fault::without_relay`) every such row, and no other,
//! fails with `VerifyError::EnsureCleanupLostOnNlr`.

mod common;

use beamtalk_codegen::core_erlang::ensure_nlr_relay::fault::without_relay;
use beamtalk_codegen::core_erlang::repl_nlr::boundary_fault::without_boundary;
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{Severity, lex_with_eof, parse};
use beamtalk_repl::codegen::{
    generate_repl_expressions, generate_repl_expressions_surfacing_verifier,
    generate_repl_expressions_traced,
};
use common::erlc_batched;
use std::collections::HashMap;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::process::Command;

/// How a loop kind runs its body.
#[derive(Clone, Copy)]
enum Iteration {
    /// `x` is the block parameter, over 1, 2, 3.
    Param,
    /// `x` is a workspace binding the loop counts from 0 to 3, incremented
    /// before the body (`{S}`) in each iteration.
    Binding,
}

/// The loop kinds: the loop with `{S}` for its body, and how `x` is bound.
/// The unary `[...] whileTrue`/`whileFalse` are left out: at the REPL they
/// raise `does_not_understand` whatever their body (BT-4313).
const LOOPS: &[(&str, Iteration)] = &[
    ("#(1, 2, 3) do: [:x | {S}]", Iteration::Param),
    ("#(1, 2, 3) collect: [:x | {S}. x]", Iteration::Param),
    ("#(1, 2, 3) select: [:x | {S}. true]", Iteration::Param),
    ("#(1, 2, 3) reject: [:x | {S}. false]", Iteration::Param),
    (
        "#(1, 2, 3) inject: 0 into: [:a :x | {S}. a + x]",
        Iteration::Param,
    ),
    (
        "#(1, 2, 3) detect: [:x | {S}. false] ifNone: [0]",
        Iteration::Param,
    ),
    ("#(1, 2, 3) count: [:x | {S}. true]", Iteration::Param),
    ("#(1, 2, 3) anySatisfy: [:x | {S}. false]", Iteration::Param),
    ("#(1, 2, 3) allSatisfy: [:x | {S}. true]", Iteration::Param),
    (
        "#(1, 2, 3) do: [:x | {S}] separatedBy: [nil]",
        Iteration::Param,
    ),
    ("1 to: 3 do: [:x | {S}]", Iteration::Param),
    ("1 to: 3 by: 1 do: [:x | {S}]", Iteration::Param),
    ("3 timesRepeat: [x := x + 1. {S}]", Iteration::Binding),
    ("[x < 3] whileTrue: [x := x + 1. {S}]", Iteration::Binding),
    ("[x >= 3] whileFalse: [x := x + 1. {S}]", Iteration::Binding),
];

/// What one position of the `^` does in one iteration: the next `w`, and the
/// value the `^` answers if it fires.
type Step = fn(i64, i64) -> (i64, Option<i64>);

/// The positions of the `^` in the loop body, with their reference semantics.
const POSITIONS: &[(&str, &str, Step)] = &[
    ("statement", "w := w + x. ^w", |x, w| (w + x, Some(w + x))),
    ("arm", "w := w + x. x == 2 ifTrue: [^w]", |x, w| {
        (w + x, (x == 2).then_some(w + x))
    }),
    (
        "two-armed conditional",
        "x == 2 ifTrue: [^w + 100] ifFalse: [w := w + x]",
        |x, w| {
            if x == 2 {
                (w, Some(w + 100))
            } else {
                (w + x, None)
            }
        },
    ),
    (
        "evaluated block",
        "w := w + x. x == 2 ifTrue: [[^w] value]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "nested loop",
        "w := w + x. #(10) do: [:y | x == 2 ifTrue: [^w + y]]",
        |x, w| (w + x, (x == 2).then_some(w + x + 10)),
    ),
    (
        "protected block",
        "w := w + x. [x == 2 ifTrue: [^w]] on: Error do: [:e | 0]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "ensure: block",
        "w := w + x. [x == 2 ifTrue: [^w]] ensure: [nil]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    // The cleanup runs in every iteration, the one the `^` ends included: its
    // write reaches the eval's boundary with the throw.
    (
        "ensure: cleanup writes",
        "[x == 2 ifTrue: [^w + x]] ensure: [w := w + x]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    // BT-4424: the cleanup writes a binding the loop body wrote before the
    // `ensure:`, which the body binds in the generated code: the cleanup
    // reads it back on entry as a member of the bindings map, the frame it
    // runs over (it tripped `LocalRebindModeMismatch` before the fix).
    (
        "ensure: cleanup writes a loop-written binding",
        "w := w + x. [x == 2 ifTrue: [^w]] ensure: [w := w + 100]",
        |x, w| (w + x + 100, (x == 2).then_some(w + x)),
    ),
    (
        "ensure: cleanup writes a loop-written binding, never fires",
        "w := w + x. [x == 5 ifTrue: [^w]] ensure: [w := w + 100]",
        |x, w| (w + x + 100, None),
    ),
    // BT-4424: the protected block writes the binding too; the cleanup reads
    // the protected block's write, not the value bound before the `ensure:`.
    // (Its scope holds a `^`, so the writing cleanup keeps its relay.)
    (
        "ensure: cleanup writes after its protected block writes a loop-written binding",
        "w := w + x. [w := w * 2] ensure: [w := w + 100]. x == 5 ifTrue: [^w]",
        |x, w| ((w + x) * 2 + 100, None),
    ),
    // BT-4424 review: the cleanup writes a binding the body bound without
    // writing it to the bindings map (a destructured name, BT-4439): its
    // scratch slot is seeded under the binding's own key, so the cleanup's
    // read of it never misses (`badkey`).
    (
        "ensure: cleanup writes a destructured binding",
        "#(v, b) := #(x, 0). [x == 5 ifTrue: [^w]] ensure: [v := v + 1. w := w + v]",
        |x, w| (w + x + 1, None),
    ),
    // BT-4439: a destructuring assignment writes each binding it binds, as
    // `w := ...` does: the next iteration, the `^` and the eval read it.
    (
        "destructuring write",
        "#(w, b) := #(w + x, 0). x == 2 ifTrue: [^w]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "destructuring write, never fires",
        "#(w, b) := #(w + x, 0). x == 5 ifTrue: [^w]",
        |x, w| (w + x, None),
    ),
    // BT-4439: an `ensure:` cleanup writes the destructured binding
    // afterwards, reading the destructuring's write.
    (
        "ensure: cleanup writes a destructured binding afterwards",
        "#(w, b) := #(w + x, 0). [x == 5 ifTrue: [^w]] ensure: [w := w + 100]",
        |x, w| (w + x + 100, None),
    ),
    (
        "match: arm",
        "w := w + x. x match: [2 -> ^w; _ -> nil]",
        |x, w| (w + x, (x == 2).then_some(w + x)),
    ),
    (
        "before a write",
        "x == 2 ifTrue: [^w]. w := w + x",
        |x, w| {
            if x == 2 { (w, Some(w)) } else { (w + x, None) }
        },
    ),
    (
        "assigned value",
        "x == 2 ifTrue: [^w := w + 10]. w := w + x",
        |x, w| {
            if x == 2 {
                (w + 10, Some(w + 10))
            } else {
                (w + x, None)
            }
        },
    ),
    // A body that writes no binding: the throw carries the bindings the
    // loop was entered with.
    ("no write", "x == 2 ifTrue: [^x * 7]", |x, w| {
        (w, (x == 2).then_some(14))
    }),
    ("never fires", "w := w + x. x == 5 ifTrue: [^w]", |x, w| {
        (w + x, (x == 5).then_some(w + x))
    }),
];

/// The positions of the loop (`{L}`) in the input, with the value `w` has
/// before the loop runs and the value of `w` and of the input when the `^`
/// never fires (`None`: the input's value is the loop's own, not checked).
const CONTEXTS: &[(&str, &str, i64, Option<i64>)] = &[
    ("after a write", "w := 5\n{L}", 5, None),
    ("alone", "{L}", 0, None),
    ("before a statement", "{L}\nw := 99", 0, Some(99)),
    ("operand", "#[0, ({L})]", 0, None),
    ("conditional arm", "true ifTrue: [{L}]", 0, None),
];

/// One generated input.
struct Row {
    name: String,
    source: String,
    expected: Expected,
}

/// The reference answer of a row: the input's value (`None` when not
/// checked) and the bindings `w` and `x` after it.
#[derive(Debug, PartialEq, Eq)]
struct Expected {
    fired: bool,
    value: Option<i64>,
    w: i64,
    x: Option<i64>,
}

/// The reference semantics: the method's. The loop runs its body over
/// `x = 1, 2, 3` until the `^` fires; the `^` ends the input with its value,
/// and every binding written before it keeps its value.
fn expected(iteration: Iteration, step: Step, w0: i64, unfired_w: Option<i64>) -> Expected {
    let mut w = w0;
    for x in 1..=3 {
        let (next, fired) = step(x, w);
        w = next;
        if let Some(value) = fired {
            return Expected {
                fired: true,
                value: Some(value),
                w,
                x: matches!(iteration, Iteration::Binding).then_some(x),
            };
        }
    }
    Expected {
        fired: false,
        value: unfired_w,
        w: unfired_w.unwrap_or(w),
        x: matches!(iteration, Iteration::Binding).then_some(3),
    }
}

/// Every generated input the compiler accepts. A row whose loop writes a
/// binding where a top-level expression does not thread the write back yet
/// (BT-3743: a loop that is a literal element) is rejected before it compiles,
/// for that reason and no other, and left out.
fn rows() -> Vec<Row> {
    let rows = all_rows();
    let total = rows.len();
    let accepted: Vec<Row> = rows
        .into_iter()
        .filter(|row| match parse_input(&row.source) {
            Ok(_) => true,
            Err(e) if e.contains("(BT-3743)") => false,
            Err(e) => panic!("{}: rejected:\n{}\n{e}", row.name, row.source),
        })
        .collect();
    assert!(
        accepted.len() + 5 >= total,
        "{} of {total} rows are rejected (BT-3743)",
        total - accepted.len()
    );
    accepted
}

fn all_rows() -> Vec<Row> {
    let mut rows = Vec::new();
    let mut push = |loop_index: usize, position: usize, context: usize| {
        let (lp, iteration) = LOOPS[loop_index];
        let (pos_name, body, step) = POSITIONS[position];
        let (ctx_name, template, w0, unfired_w) = CONTEXTS[context];
        let loop_src = lp.replace("{S}", body);
        let source = template.replace("{L}", &loop_src);
        rows.push(Row {
            name: format!("{lp} / {pos_name} / {ctx_name}"),
            source,
            expected: expected(iteration, step, w0, unfired_w),
        });
    };
    for loop_index in 0..LOOPS.len() {
        // Every position after a binding write in the same input...
        for position in 0..POSITIONS.len() {
            push(loop_index, position, 0);
        }
        // ... and the arm position in every other loop position.
        for context in 1..CONTEXTS.len() {
            push(loop_index, 1, context);
        }
    }
    rows
}

/// What the `ensure:` cleanups a shape's `^` runs through on its way to the
/// eval's boundary do to `w`, from the value `w` has when the `^` runs
/// (`None`: no cleanup writes `w`).
type Cleanup = Option<fn(i64) -> i64>;

/// BT-4312: the shapes of a `^` outside any loop, `{V}` its value: whether
/// it fires, and what the `ensure:` cleanups it runs through write. The guard
/// reads `x`, which every row starts at 0. A cleanup's writes reach the
/// boundary with the throw, in every scope: `w := 0. [^5] ensure: [w := 9]`
/// leaves `w` at 9, as the bare `^` did before BT-4312.
const ROOT_SHAPES: &[(&str, &str, bool, Cleanup)] = &[
    ("root", "^{V}", true, None),
    ("arm", "x == 0 ifTrue: [^{V}]", true, None),
    ("ifFalse: arm", "x == 1 ifFalse: [^{V}]", true, None),
    (
        "two-armed, first",
        "x == 0 ifTrue: [^{V}] ifFalse: [0]",
        true,
        None,
    ),
    (
        "two-armed, second",
        "x == 1 ifTrue: [0] ifFalse: [^{V}]",
        true,
        None,
    ),
    (
        "nested arm",
        "x == 0 ifTrue: [x < 5 ifTrue: [^{V}]]",
        true,
        None,
    ),
    ("and: block", "(x == 0 and: [^{V}]) ifTrue: [0]", true, None),
    ("or: block", "(x == 1 or: [^{V}]) ifTrue: [0]", true, None),
    ("ifNil: arm", "nil ifNil: [^{V}]", true, None),
    ("ifNotNil: arm", "x ifNotNil: [:v | ^{V}]", true, None),
    (
        "lookup arm",
        "#{#a => 1} at: #b ifAbsent: [^{V}]",
        true,
        None,
    ),
    ("tryDo: block", "Result tryDo: [^{V}]", true, None),
    ("evaluated block", "[^{V}] value", true, None),
    (
        "arm in an evaluated block",
        "[x == 0 ifTrue: [^{V}]] value",
        true,
        None,
    ),
    (
        "protected block",
        "[^{V}] on: Error do: [:e | 0]",
        true,
        None,
    ),
    ("handler", "[1 / 0] on: Error do: [:e | ^{V}]", true, None),
    ("ensure: block", "[^{V}] ensure: [nil]", true, None),
    ("ensure: cleanup", "[0] ensure: [^{V}]", true, None),
    ("match: arm", "x match: [0 -> ^{V}; _ -> nil]", true, None),
    ("operand", "#[0, (x == 0 ifTrue: [^{V}])]", true, None),
    ("never fires", "x == 5 ifTrue: [^{V}]", false, None),
    // The `ensure:` cleanups the throw runs write a binding.
    (
        "ensure: cleanup writes",
        "[^{V}] ensure: [w := 9]",
        true,
        Some(|_| 9),
    ),
    (
        "ensure: cleanup reads and writes",
        "[^{V}] ensure: [w := w + 100]",
        true,
        Some(|w| w + 100),
    ),
    (
        "ensure: cleanup loop",
        "[^{V}] ensure: [#(1, 2) do: [:y | w := w + y]]",
        true,
        Some(|w| w + 3),
    ),
    (
        "ensure: cleanup arm",
        "[^{V}] ensure: [x == 0 ifTrue: [w := w + 100]]",
        true,
        Some(|w| w + 100),
    ),
    // An outer cleanup that read `w` would be rejected (ADR 0133: its
    // protected block writes `w` and holds a `^`), so it only writes.
    (
        "nested ensure: cleanups",
        "[[^{V}] ensure: [w := w + 100]] ensure: [w := 7]",
        true,
        Some(|_| 7),
    ),
    (
        "ensure: inside a non-writing ensure:",
        "[[^{V}] ensure: [w := w + 100]] ensure: [nil]",
        true,
        Some(|w| w + 100),
    ),
    (
        "ensure: in a cleanup",
        "[^{V}] ensure: [[w := w + 100] ensure: [w := w * 2]]",
        true,
        Some(|w| (w + 100) * 2),
    ),
    (
        "on:do: inside ensure:",
        "[[^{V}] on: Error do: [:e | 0]] ensure: [w := 9]",
        true,
        Some(|_| 9),
    ),
    (
        "ensure: inside on:do:",
        "[[^{V}] ensure: [w := 9]] on: Error do: [:e | 0]",
        true,
        Some(|_| 9),
    ),
    (
        "ensure: in an arm",
        "x == 0 ifTrue: [[^{V}] ensure: [w := w + 100]]",
        true,
        Some(|w| w + 100),
    ),
    (
        "ensure: in an evaluated block",
        "[[x == 0 ifTrue: [^{V}]] ensure: [w := w + 100]] value",
        true,
        Some(|w| w + 100),
    ),
];

/// BT-4312: a position of the `^` (`{R}`) in the input.
struct RootContext {
    name: &'static str,
    template: &'static str,
    /// The value `w` has when the `^` runs.
    w_at: i64,
    /// The input's value when the `^` does not fire (`None`: the last
    /// statement's own value, not checked).
    unfired_value: Option<i64>,
    /// The bindings `w` and `x` when the `^` does not fire.
    unfired_w: i64,
    unfired_x: i64,
}

/// BT-4312: the positions of the `^` in the input.
const ROOT_CONTEXTS: &[RootContext] = &[
    RootContext {
        name: "between writes",
        template: "w := 1\n{R}\nw := 9\nx := 2",
        w_at: 1,
        unfired_value: Some(2),
        unfired_w: 9,
        unfired_x: 2,
    },
    RootContext {
        name: "first",
        template: "{R}\nw := 9",
        w_at: 0,
        unfired_value: Some(9),
        unfired_w: 9,
        unfired_x: 0,
    },
    RootContext {
        name: "same line",
        template: "w := 1. {R}. w := 9. x := 2",
        w_at: 1,
        unfired_value: Some(2),
        unfired_w: 9,
        unfired_x: 2,
    },
    RootContext {
        name: "last",
        template: "w := 1\n{R}",
        w_at: 1,
        unfired_value: None,
        unfired_w: 1,
        unfired_x: 0,
    },
    RootContext {
        name: "alone",
        template: "{R}",
        w_at: 0,
        unfired_value: None,
        unfired_w: 0,
        unfired_x: 0,
    },
    RootContext {
        name: "in an arm",
        template: "w := 1\ntrue ifTrue: [{R}. w := 9]\nx := 2",
        w_at: 1,
        unfired_value: Some(2),
        unfired_w: 9,
        unfired_x: 2,
    },
    RootContext {
        name: "after a loop",
        template: "#(1, 2) do: [:y | w := w + y]\n{R}\nx := 2",
        w_at: 3,
        unfired_value: Some(2),
        unfired_w: 3,
        unfired_x: 2,
    },
];

/// One BT-4312 row: the generated input and its reference answer, and
/// whether its `^` is the input's whole last statement (which throws to no
/// boundary: it already ends the input).
struct RootRow {
    row: Row,
    final_root: bool,
    /// The `^` runs an `ensure:` cleanup that writes a binding.
    cleanup_writes: bool,
}

/// Every BT-4312 row: each shape in each context. The `^` answers `w + 10`.
fn root_rows() -> Vec<RootRow> {
    let mut rows = Vec::new();
    for (shape_name, shape, fires, cleanup) in ROOT_SHAPES {
        for ctx in ROOT_CONTEXTS {
            let source = ctx.template.replace("{R}", &shape.replace("{V}", "w + 10"));
            let expected = if *fires {
                Expected {
                    fired: true,
                    value: Some(ctx.w_at + 10),
                    w: cleanup.map_or(ctx.w_at, |f| f(ctx.w_at)),
                    x: Some(0),
                }
            } else {
                Expected {
                    fired: false,
                    value: ctx.unfired_value,
                    w: ctx.unfired_w,
                    x: Some(ctx.unfired_x),
                }
            };
            let final_root = *shape_name == "root" && matches!(ctx.name, "last" | "alone");
            rows.push(RootRow {
                row: Row {
                    name: format!("{shape_name} / {}", ctx.name),
                    source,
                    expected,
                },
                final_root,
                cleanup_writes: cleanup.is_some(),
            });
        }
    }
    rows
}

/// The workspace bindings every row starts from.
const KNOWN: &[&str] = &["w", "x"];

/// The input's expressions, or the first message of the compile errors that
/// reject it.
fn parse_input(src: &str) -> Result<Vec<Expression>, String> {
    let (parsed, diags) = parse(lex_with_eof(src));
    assert!(diags.is_empty(), "parse {src}: {diags:?}");
    let analysis = analyse_full(&parsed, AnalysisContext::default().with_known_vars(KNOWN));
    if let Some(error) = analysis
        .diagnostics
        .iter()
        .find(|d| d.severity == Severity::Error)
    {
        return Err(error.message.to_string());
    }
    Ok(parsed
        .expressions
        .into_iter()
        .map(|s| s.expression)
        .collect())
}

fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
    payload
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
        .unwrap_or_default()
}

/// Generates `src` as eval (`trace: false`) or trace module `module`. A
/// verifier finding (a debug panic) or a codegen error is an `Err`.
fn generate(src: &str, module: &str, trace: bool) -> Result<String, String> {
    let expressions = parse_input(src).map_err(|e| format!("rejected: {e}"))?;
    match catch_unwind(AssertUnwindSafe(|| {
        if trace {
            generate_repl_expressions_traced(&expressions, src, module, HashMap::new())
        } else {
            generate_repl_expressions(&expressions, module)
        }
    })) {
        Ok(Ok(code)) => Ok(code),
        Ok(Err(e)) => Err(format!("codegen: {e:?}")),
        Err(payload) => Err(format!("panic: {}", panic_message(payload.as_ref()))),
    }
}

/// The `ThreadedIr` verifier's findings on `src`, in a debug build (where a
/// finding fails generation with a panic) and a release build (where it is
/// an `internal:` warning beside the generated code) alike.
fn verifier_report(src: &str) -> String {
    let expressions = match parse_input(src) {
        Ok(expressions) => expressions,
        Err(e) => return format!("rejected: {e}"),
    };
    match catch_unwind(AssertUnwindSafe(|| {
        generate_repl_expressions_surfacing_verifier(
            &expressions,
            &beamtalk_core::ast::PipelineSends::default(),
            "bt4288_off",
            HashMap::new(),
            None,
            false,
        )
    })) {
        Ok(Ok((_, diagnostics))) => diagnostics
            .iter()
            .map(|d| d.message.to_string())
            .collect::<Vec<_>>()
            .join("\n"),
        Ok(Err(e)) => format!("codegen: {e:?}"),
        Err(payload) => format!("panic: {}", panic_message(payload.as_ref())),
    }
}

fn erlang_available() -> bool {
    Command::new("erlc").arg("-help").output().is_ok()
        && Command::new("erl").arg("-version").output().is_ok()
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("repo root")
        .to_path_buf()
}

/// The `ebin` directory of every application the runtime build produced,
/// led by the built stdlib's (its `bt@stdlib@*` class modules and the
/// generated `beamtalk_stdlib.app` that loads them, found before the rebar3
/// one). Fails
/// when the runtime or the stdlib is not built, so the execution proof cannot
/// pass vacuously.
fn runtime_ebins() -> Vec<PathBuf> {
    let root = repo_root();
    let lib = root.join("runtime/_build/default/lib");
    let stdlib = root.join("runtime/apps/beamtalk_stdlib/ebin");
    for built in [
        lib.join("beamtalk_runtime/ebin/beamtalk_runtime.app"),
        stdlib.join("beamtalk_stdlib.app"),
        stdlib.join("bt@stdlib@block.beam"),
    ] {
        assert!(
            built.exists(),
            "{} is not built: run `just build` first",
            built.display()
        );
    }
    let mut ebins: Vec<PathBuf> = std::fs::read_dir(&lib)
        .expect("runtime lib dir")
        .filter_map(Result::ok)
        .map(|entry| entry.path().join("ebin"))
        .filter(|ebin| ebin.is_dir())
        .collect();
    ebins.sort();
    ebins.insert(0, stdlib);
    ebins
}

/// Runs `eval/1` of every module in `modules` (compiled into `dir`; `true`
/// for a trace module) on a node with the stdlib started, from the bindings
/// `w => 0, x => 0`, and answers per module the input's value (a trace
/// module's last step's, with the step count) and the bindings `w` and `x`.
///
/// In batches: the `-eval` script lists every module, and one call for the
/// whole corpus approaches the Windows ~32K command-line limit (BT-4224).
/// Batches of 128 keep every invocation to a few kilobytes on every
/// platform.
fn run_on_beam(dir: &Path, modules: &[(String, bool)]) -> HashMap<String, String> {
    const BATCH: usize = 128;
    let mut merged = HashMap::new();
    for chunk in modules.chunks(BATCH) {
        let list = chunk
            .iter()
            .map(|(m, trace)| format!("{{'{m}', {trace}}}"))
            .collect::<Vec<_>>()
            .join(",");
        let script = format!(
            "{{ok, _}} = application:ensure_all_started(beamtalk_stdlib), \
             lists:foreach(fun({{M, Trace}}) -> \
               Out = try {{Trace, M:eval(#{{w => 0, x => 0}})}} of \
                 {{true, {{Steps, B}}}} -> \
                   {{_, V}} = lists:last(Steps), \
                   io_lib:format(\"~w ~w ~w steps=~w\", [V, maps:get(w, B), maps:get(x, B), length(Steps)]); \
                 {{false, {{V, B}}}} -> \
                   io_lib:format(\"~w ~w ~w\", [V, maps:get(w, B), maps:get(x, B)]) \
               catch C:E:St -> io_lib:format(\"error ~w:~0p ~0p\", [C, E, hd(St)]) end, \
               io:format(\"~s => ~s~n\", [M, Out]) \
             end, [{list}]), halt().",
        );
        let mut cmd = Command::new("erl");
        // The node runs in `dir`, so a crash dump never lands in the source tree.
        cmd.current_dir(dir).arg("-noshell").arg("-pa").arg(dir);
        for ebin in runtime_ebins() {
            cmd.arg("-pa").arg(ebin);
        }
        let output = cmd.arg("-eval").arg(script).output().expect("run erl");
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            output.status.success(),
            "erl failed:\n{stdout}{}",
            String::from_utf8_lossy(&output.stderr)
        );
        merged.extend(
            stdout
                .lines()
                .filter_map(|l| l.split_once(" => "))
                .map(|(m, out)| (m.to_string(), out.to_string())),
        );
    }
    merged
}

/// Whether the `erl` output `out` of a module agrees with `expected`.
/// `throws`: the row's `^` throws to the eval's boundary (every row's but a
/// `^` that is the input's whole last statement).
fn agrees(out: &str, expected: &Expected, trace: bool, throws: bool) -> bool {
    let mut parts = out.split(' ');
    let (Some(value), Some(w), Some(x)) = (parts.next(), parts.next(), parts.next()) else {
        return false;
    };
    let value_ok = expected.value.is_none_or(|v| value == v.to_string());
    let w_ok = w == expected.w.to_string();
    let x_ok = x == expected.x.unwrap_or(0).to_string();
    // A `^` ends a traced input with one step: the input and the `^`'s value.
    let steps_ok = !(trace && expected.fired && throws) || parts.next() == Some("steps=1");
    value_ok && w_ok && x_ok && steps_ok
}

/// Proofs 1 and 2 over `rows` (each with whether its `^` throws to the eval's
/// boundary): every row compiles through the verifier and `erlc`, in eval and
/// trace mode; without its boundary a row whose `^` throws fails
/// `ReplReturnWithoutBoundary`, and one whose `^` does not compiles no throw.
/// Answers the failures.
fn compile_proofs(rows: &[(&Row, bool)], prefix: &str) -> Vec<String> {
    let mut failures = Vec::new();
    let mut modules = Vec::new();
    for (i, (row, throws)) in rows.iter().enumerate() {
        for trace in [false, true] {
            let module = format!("{prefix}_c{}_{i}", u8::from(trace));
            match generate(&row.source, &module, trace) {
                Ok(code) if !throws && code.contains("'$bt_nlr'") => failures.push(format!(
                    "{} (trace: {trace}): a final root `^` compiled to a throw",
                    row.name
                )),
                Ok(code) => modules.push((module, code)),
                Err(e) => failures.push(format!("{} (trace: {trace}): {e}", row.name)),
            }
        }
        let report = without_boundary(|| verifier_report(&row.source));
        if *throws != report.contains("ReplReturnWithoutBoundary") {
            failures.push(format!(
                "{}: without the boundary, expected ReplReturnWithoutBoundary: {throws}, got {report}",
                row.name
            ));
        }
    }
    if erlang_available() {
        let dir = beamtalk_core::test_helpers::unique_temp_dir(&format!("{prefix}_compile"));
        std::fs::create_dir_all(&dir).expect("temp dir");
        if let Err(e) = erlc_batched(&dir, &modules) {
            failures.push(format!("erlc: {e}"));
        }
        let _ = std::fs::remove_dir_all(&dir);
    }
    failures
}

/// Proof 3 over `rows` (as for [`compile_proofs`]): every row answers, in
/// eval and trace mode, what the method semantics says. Answers the
/// failures.
fn run_proofs(rows: &[(&Row, bool)], prefix: &str) -> Vec<String> {
    let mut modules = Vec::new();
    for (i, (row, _)) in rows.iter().enumerate() {
        for trace in [false, true] {
            let module = format!("{prefix}_r{}_{i}", u8::from(trace));
            let code = generate(&row.source, &module, trace)
                .unwrap_or_else(|e| panic!("{}: {e}", row.name));
            modules.push((module, code));
        }
    }
    let dir = beamtalk_core::test_helpers::unique_temp_dir(&format!("{prefix}_run"));
    std::fs::create_dir_all(&dir).expect("temp dir");
    erlc_batched(&dir, &modules).unwrap_or_else(|e| panic!("erlc: {e}"));
    let trace_prefix = format!("{prefix}_r1_");
    let names: Vec<(String, bool)> = modules
        .iter()
        .map(|(m, _)| (m.clone(), m.starts_with(&trace_prefix)))
        .collect();
    let outputs = run_on_beam(&dir, &names);
    let _ = std::fs::remove_dir_all(&dir);
    let mut failures = Vec::new();
    for (i, (row, throws)) in rows.iter().enumerate() {
        for trace in [false, true] {
            let module = format!("{prefix}_r{}_{i}", u8::from(trace));
            let out = outputs.get(&module).map_or("<no output>", String::as_str);
            if !agrees(out, &row.expected, trace, *throws) {
                failures.push(format!(
                    "{} (trace: {trace}):\n{}\n  expected {:?}, got {out}",
                    row.name, row.source, row.expected
                ));
            }
        }
    }
    failures
}

/// Proofs 1 and 2 for the loop-body corpus.
#[test]
fn repl_loop_body_return_corpus_compiles_and_needs_its_boundary() {
    let rows = rows();
    let rows: Vec<(&Row, bool)> = rows.iter().map(|row| (row, true)).collect();
    let failures = compile_proofs(&rows, "bt4288");
    assert!(
        failures.is_empty(),
        "{} of {} REPL loop-body `^` row(s) fail:\n\n{}",
        failures.len(),
        rows.len(),
        failures.join("\n\n")
    );
}

/// Proof 3 for the loop-body corpus.
#[test]
fn repl_loop_body_returns_answer_like_a_method() {
    if !erlang_available() {
        eprintln!("erl/erlc not installed: skipping the BEAM run");
        return;
    }
    let rows = rows();
    assert!(
        rows.len() >= 220,
        "the corpus has thinned out: {}",
        rows.len()
    );
    assert!(
        rows.iter().filter(|r| r.expected.fired).count() >= 190,
        "too few rows fire their `^`"
    );
    let rows: Vec<(&Row, bool)> = rows.iter().map(|row| (row, true)).collect();
    let failures = run_proofs(&rows, "bt4288");
    assert!(
        failures.is_empty(),
        "{} REPL loop-body `^` run(s) disagree with the method semantics:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4312: proofs 1 and 2 for the root and arm corpus ([`root_rows`]).
#[test]
fn repl_root_and_arm_return_corpus_compiles_and_needs_its_boundary() {
    let rows = root_rows();
    let rows: Vec<(&Row, bool)> = rows.iter().map(|r| (&r.row, !r.final_root)).collect();
    let failures = compile_proofs(&rows, "bt4312");
    assert!(
        failures.is_empty(),
        "{} of {} REPL root or arm `^` row(s) fail:\n\n{}",
        failures.len(),
        rows.len(),
        failures.join("\n\n")
    );
}

/// BT-4312: proof 3 for the root and arm corpus. Before the fix every row
/// that fires its `^` before the input's last statement ran on (`w := 1.
/// ^w + 10. w := 9. x := 2` answered 2).
#[test]
fn repl_root_and_arm_returns_answer_like_a_method() {
    if !erlang_available() {
        eprintln!("erl/erlc not installed: skipping the BEAM run");
        return;
    }
    let rows = root_rows();
    assert!(
        rows.len() >= ROOT_SHAPES.len() * ROOT_CONTEXTS.len(),
        "the corpus has thinned out: {}",
        rows.len()
    );
    let rows: Vec<(&Row, bool)> = rows.iter().map(|r| (&r.row, !r.final_root)).collect();
    let failures = run_proofs(&rows, "bt4312");
    assert!(
        failures.is_empty(),
        "{} REPL root or arm `^` run(s) disagree with the method semantics:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4312: proof 2 for the `ensure:` relay. Without it (the catch re-raises
/// the throw after the cleanup, as before the fix), every row whose `^` runs
/// an `ensure:` cleanup that writes a binding fails
/// `VerifyError::EnsureCleanupLostOnNlr`, the check that fails on the repro
/// before the fix, and no other row does. Proof 3 (the BEAM runs above) shows
/// the cleanup's write reaches the eval's boundary with the relay.
#[test]
fn ensure_cleanups_a_return_runs_need_their_relay() {
    let root = root_rows();
    let loops = rows();
    let all = root.iter().map(|r| (&r.row, r.cleanup_writes)).chain(
        loops
            .iter()
            .map(|r| (r, r.name.contains("ensure: cleanup writes"))),
    );
    let mut failures = Vec::new();
    let mut relayed = 0;
    for (row, writes) in all {
        let report = without_relay(|| verifier_report(&row.source));
        if writes != report.contains("EnsureCleanupLostOnNlr") {
            failures.push(format!(
                "{}: without the relay, expected EnsureCleanupLostOnNlr: {writes}, got {report}",
                row.name
            ));
        }
        relayed += usize::from(writes);
    }
    assert!(
        relayed >= 80,
        "too few rows run a writing cleanup: {relayed}"
    );
    assert!(
        failures.is_empty(),
        "{} row(s) disagree:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4312: an `ensure:` whose receiver or cleanup is not a block literal
/// runs its cleanup as a closure. At the REPL such a closure can never write
/// the bindings map a `^` of the eval carries, so the catch has nothing to
/// relay: a block that assigns a binding cannot be stored (ADR 0133), and a
/// `^` in a stored block answers its value where it stands, never throwing to
/// the eval's boundary. The method scopes, where the closure writes the
/// actor-state cell, are `beamtalk-codegen`'s `ensure_relay_corpus.rs`.
#[test]
fn a_closure_cleanup_never_writes_the_bindings_a_return_carries() {
    for src in [
        "c := [w := 9]. [^5] ensure: c",
        "c := [w := w + 1]. [x == 0 ifTrue: [^5]] ensure: c",
    ] {
        match parse_input(src) {
            Err(e) if e.contains("assigned in a block stored in") => {}
            other => panic!("{src}: expected the ADR 0133 rejection, got {other:?}"),
        }
    }
    for src in [
        "b := [^5]. b ensure: [nil]. x := 2",
        "b := [^5]. [b value] ensure: [w := 9]. x := 2",
    ] {
        let code = generate(src, "bt4312_closure", false).unwrap_or_else(|e| panic!("{src}: {e}"));
        assert!(
            !code.contains("'$bt_nlr'"),
            "{src}: a stored block's `^` compiled to a throw:\n{code}"
        );
    }
}

/// BT-4312 (A1): a `^` run in place inside the value of a top-level
/// assignment is rejected before it compiles, as BT-4288's loop case is: a
/// method never assigns the variable, but the workspace would bind it to the
/// input's value.
#[test]
fn a_return_in_place_in_a_top_level_assignment_is_rejected() {
    for src in [
        "r := true ifTrue: [^3]",
        "r := x == 0 ifTrue: [^3] ifFalse: [4]",
        "w := 1\nr := [^w] value",
        "r := [^3] on: Error do: [:e | 0]",
        "r := [^3] ensure: [nil]",
        "#(a, b) := true ifTrue: [^#(1, 2)]",
        "r := #(1, 2) do: [:y | ^y]",
    ] {
        match parse_input(src) {
            Err(e) if e.contains("never completes") => {}
            other => panic!("{src}: expected the BT-4312 rejection, got {other:?}"),
        }
    }
}

/// A `^` whose loop is held in a closure never throws to the REPL eval's
/// boundary: the closure may run in a later input, after the eval that created
/// it has ended, where nothing catches the throw. As on main, such a loop is
/// rejected (BT-4003); a closure created in a loop body and holding a `^` of
/// its own is accepted and answers its value where it stands, so its code
/// holds no non-local-return throw at all. The repl-protocol case
/// `bt4003_repl_returns.btscript` calls such a closure from a later input.
#[test]
fn a_return_in_a_loop_held_by_a_closure_never_throws_to_the_eval() {
    for src in [
        "b := [#(1, 2) do: [:x | ^x]]",
        "Holder keep: [#(1, 2) do: [:x | ^x]]",
        "true ifTrue: [b := [#(1, 2) do: [:x | ^x]]]",
        "#(1, 2) do: [:x | b := [#(3) do: [:y | ^y]]]",
    ] {
        match parse_input(src) {
            Err(e) if e.contains("loop would ignore it") => {}
            other => panic!("{src}: expected the BT-4003 rejection, got {other:?}"),
        }
    }
    for src in [
        "#(1, 2) do: [:z | b := [:y | ^y * z]]",
        "w := 0\n#(1, 2) do: [:x | w := w + x. b := [:y | ^y + 1]]",
        "#(1, 2) collect: [:x | [:y | ^y]]",
    ] {
        let code = generate(src, "bt4288_closure", false).unwrap_or_else(|e| panic!("{src}: {e}"));
        assert!(
            !code.contains("'$bt_nlr'"),
            "{src}: a closure's `^` compiled to a throw:\n{code}"
        );
    }
}
