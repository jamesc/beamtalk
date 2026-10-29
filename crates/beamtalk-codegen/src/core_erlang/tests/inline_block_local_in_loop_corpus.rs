// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3947 (closes BT-3948): a literal block applied in place
//! (`[:z | w := z. w] value: i`, `[v := 1. v] value`) that assigns a local of
//! its own, in a loop or fold body or a `whileTrue:`/`whileFalse:` condition,
//! compiles in every method context.
//!
//! BT-3948 found the actor shapes failing `erlc` on an unbound `StateAcc` or
//! answering a `{Value, Locals}` carrier, and made them a compile error
//! (`Residual::InlineBlockLocalInLoop`): the block body's assignment to its own
//! local was lowered as a `Put` into a scratch map that a direct-params loop
//! frame (its locals are the loop's parameters) does not have. Two frame rules
//! own that now, and BT-3947 deleted the row:
//!
//! - the evaluated block is a producer in the enclosing frame (BT-3982,
//!   `generate_evaluated_block_producer`), its body a `BranchArm` frame that
//!   seeds its own scratch map where the enclosing frame has none
//!   (`conditional_has_private_scratch`);
//! - an assignment to a local its arm frame does not carry is a plain `let`,
//!   never a scratch-map `Put` (BT-3911, `VerifyError::ArmLocalPutNotCarried`).
//!
//! Three proofs over a generated grid (evaluated block x loop or fold x
//! statement position in the body, or a loop-condition position, x enclosing
//! host x method context):
//!
//! 1. no cell is rejected by the semantic checks for the deleted BT-3948 rule,
//!    and almost every cell is accepted;
//! 2. every accepted cell compiles through the debug `ThreadedIr` verifier,
//!    and each (context, host) module through `erlc`;
//! 3. with the BT-3948 lowering put back ([`LocalRebindFault::PutUncarriedLocal`]:
//!    every assignment gets the scratch-map `Put`), every actor cell whose
//!    block is lowered in place (the evaluated-block producer, or an arm of
//!    another producer) fails `ArmLocalPutNotCarried`, the check that fails
//!    on the repro before the fix.
//!
//! The runtime answers are `BUnit` cells:
//! `stdlib/test/bt3947inline_block_loop_test.bt`.

use super::{assert_compiles_through_erlc, codegen, erlc_available, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use std::sync::OnceLock;

/// Evaluated blocks; `{a}` is the argument the loop supplies. All but the last
/// assign a local of their own (or a parameter); the last writes nothing
/// (BT-4266's shape).
const BLOCKS: &[&str] = &[
    "[:z | w := z. w] value: {a}",
    "[:z | w := 0. z > 0 ifTrue: [w := z]. w] value: {a}",
    "[v := {a}. v] value",
    "[:z :y | v := z + y. v] value: {a} value: 1",
    "[[m := {a}. m] value] value",
    "[:z | z + 1] value: {a}",
];

/// The blocks of [`BLOCKS`] that assign a local of their own.
const ASSIGNING_BLOCKS: usize = 5;

/// Loops and folds with the body statement `{s}` and the argument `{a}` it
/// passes to the block.
const LOOPS: &[(&str, &str)] = &[
    ("1 to: 3 do: [:i | {s}]", "i"),
    ("3 timesRepeat: [{s}]", "1"),
    ("[t < 9] whileTrue: [t := t + 1. {s}]", "t"),
    ("[t > 9] whileFalse: [t := t + 1. {s}]", "t"),
    ("#(1, 2) do: [:i | {s}]", "i"),
    ("#(1, 2) inject: 0 into: [:acc :i | {s}. acc]", "i"),
];

/// Statement positions of the block `{e}` in a loop body. `{st}` is a field
/// write; a value type has none.
const POSITIONS: &[&str] = &[
    "t := t + ({e})",
    "{e}",
    "r := {e}. t := t + r",
    "q := q + 1. t := t + ({e})",
    "c ifTrue: [t := t + ({e})]",
    "c and: [({e}) > 0]",
    "c ifTrue: [r := {e}. t := t + r]",
    "{st}t := t + ({e})",
    "{st}c ifTrue: [{e}]",
];

/// A `whileTrue:`/`whileFalse:` loop with the block `{e}` (argument `t`) in its
/// condition.
const CONDITIONS: &[&str] = &[
    "[({e}) < 3] whileTrue: [t := t + 1]",
    "[{e}. t < 3] whileTrue: [t := t + 1]",
    "[t < 3 and: [({e}) > 0]] whileTrue: [t := t + 1]",
    "[({e}) > 5] whileFalse: [{st}t := t + 1]",
];

/// Where the loop sits in the method (`{l}`).
const HOSTS: &[&str] = &[
    "{l}",
    "c ifTrue: [{l}]",
    "[{l}] on: Error do: [:e | 0]",
    "#(1) do: [:k | {l}]",
    "1 to: 2 do: [:j | q := q + 1. {l}]",
    "[{l}] value",
];

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Context {
    Class,
    Value,
    Actor,
}

const CONTEXTS: &[Context] = &[Context::Class, Context::Value, Context::Actor];

const CLASS: &str = "InlineBlockLoopProbe";

/// The class of `context`, holding `methods`.
fn module(context: Context, methods: &str) -> String {
    match context {
        Context::Class => format!("Object subclass: {CLASS}\n  classState: n = 0\n\n{methods}"),
        Context::Value => format!("Value subclass: {CLASS}\n\n{methods}"),
        Context::Actor => format!("Actor subclass: {CLASS}\n  state: n = 0\n\n{methods}"),
    }
}

fn method(context: Context, selector: &str, body: &str) -> String {
    let side = if context == Context::Class {
        "class "
    } else {
        ""
    };
    let body = format!("t := 0\nq := 0\nc := true\n{body}\nt");
    let indented = body
        .lines()
        .map(|l| format!("    {l}"))
        .collect::<Vec<_>>()
        .join("\n");
    format!("  {side}{selector} =>\n{indented}\n\n")
}

fn fill_state(template: &str, context: Context) -> Option<String> {
    match context {
        Context::Value if template.contains("{st}") => None,
        Context::Value => Some(template.to_string()),
        _ => Some(template.replace("{st}", "self.n := self.n + 1. ")),
    }
}

/// Every loop of the grid for `context` (body and condition positions), each
/// with whether its block assigns a local of its own.
fn loops(context: Context) -> Vec<(String, bool)> {
    let mut out = Vec::new();
    for (b, block) in BLOCKS.iter().enumerate() {
        let assigns = b < ASSIGNING_BLOCKS;
        for (lp, arg) in LOOPS {
            let e = block.replace("{a}", arg);
            for position in POSITIONS {
                if let Some(s) = fill_state(position, context) {
                    out.push((lp.replace("{s}", &s.replace("{e}", &e)), assigns));
                }
            }
        }
        let e = block.replace("{a}", "t");
        for cond in CONDITIONS {
            if let Some(c) = fill_state(cond, context) {
                out.push((c.replace("{e}", &e), assigns));
            }
        }
    }
    out
}

/// The first error the semantic checks report for `src`, if any.
fn rejection(src: &str) -> Option<String> {
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    beamtalk_core::semantic_analysis::analyse(&module)
        .diagnostics
        .into_iter()
        .find(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
        .map(|d| d.message.to_string())
}

/// One accepted cell of the grid.
struct Cell {
    context: Context,
    host: usize,
    assigns: bool,
    method: String,
}

/// The grid, checked once: its accepted cells, and the rejected cells with
/// their messages.
struct Grid {
    accepted: Vec<Cell>,
    rejected: Vec<String>,
}

fn grid() -> &'static Grid {
    static GRID: OnceLock<Grid> = OnceLock::new();
    GRID.get_or_init(|| {
        let mut accepted = Vec::new();
        let mut rejected = Vec::new();
        for &context in CONTEXTS {
            let loops = loops(context);
            for (host, template) in HOSTS.iter().enumerate() {
                for (k, (lp, assigns)) in loops.iter().enumerate() {
                    let body = template.replace("{l}", lp);
                    let method = method(context, &format!("probe{k}"), &body);
                    match rejection(&module(context, &method)) {
                        Some(message) => rejected.push(format!("{context:?}: {message}\n{body}")),
                        None => accepted.push(Cell {
                            context,
                            host,
                            assigns: *assigns,
                            method,
                        }),
                    }
                }
            }
        }
        Grid { accepted, rejected }
    })
}

/// No cell is rejected by BT-3948's deleted rule (an error naming it), and the
/// grid stays meaningful: almost every cell is accepted. The rejections left
/// are other, documented rules (the Phase 0 position check).
#[test]
fn inline_block_local_in_loop_corpus_has_no_bt3948_rejection() {
    let grid = grid();
    for r in &grid.rejected {
        assert!(!r.contains("BT-3948"), "{r}");
    }
    assert!(
        grid.accepted.len() > 20 * grid.rejected.len(),
        "{} accepted, {} rejected; first: {:?}",
        grid.accepted.len(),
        grid.rejected.len(),
        grid.rejected.first()
    );
}

/// Every accepted cell compiles through the `ThreadedIr` verifier, and each
/// (context, host) module through `erlc`.
#[test]
fn every_accepted_inline_block_local_in_loop_corpus_cell_compiles() {
    let grid = grid();
    let erlc = erlc_available();
    for &context in CONTEXTS {
        for (host, template) in HOSTS.iter().enumerate() {
            let methods: String = grid
                .accepted
                .iter()
                .filter(|c| c.context == context && c.host == host)
                .map(|c| c.method.as_str())
                .collect();
            if methods.is_empty() {
                continue;
            }
            let src = module(context, &methods);
            assert_eq!(verifier_report(&src, None), None, "{context:?} {template}");
            if erlc {
                assert_compiles_through_erlc("test", &codegen(&src));
            }
        }
    }
}

/// The BT-3948 repro, as a corpus cell.
const REPRO: &str =
    "1 to: 3 do: [:i | t := t + ([:z | w := 0. z > 0 ifTrue: [w := z]. w] value: i)]";

/// The two frame rules the deleted row stood in for, over every actor cell
/// whose block assigns a local of its own. Where the block is the
/// evaluated-block producer (with that producer switched off,
/// [`LocalRebindFault::EvaluatedBlockNotAProducer`], the cell fails
/// `UnthreadedEvaluatedBlock`), or is inlined in an arm of another producer,
/// its body's assignment to the block's own local is a plain `let`: with the
/// BT-3948 lowering put back ([`LocalRebindFault::PutUncarriedLocal`], every
/// assignment a scratch-map `Put`, whatever the frame carries) the cell fails
/// `ArmLocalPutNotCarried`. The other cells apply the block as a plain closure
/// (nothing threads).
#[test]
fn inline_block_local_in_loop_corpus_fails_arm_local_put_not_carried_under_the_bt3948_lowering() {
    let mut producers = 0;
    let mut puts = 0;
    for cell in grid()
        .accepted
        .iter()
        .filter(|c| c.context == Context::Actor && c.assigns)
    {
        let src = module(cell.context, &cell.method);
        let produced = verifier_report(&src, Some(LocalRebindFault::EvaluatedBlockNotAProducer))
            .is_some_and(|r| r.contains("UnthreadedEvaluatedBlock"));
        let put = verifier_report(&src, Some(LocalRebindFault::PutUncarriedLocal))
            .is_some_and(|r| r.contains("ArmLocalPutNotCarried"));
        assert!(
            put || !produced,
            "a producer without the arm put check\n{src}"
        );
        producers += usize::from(produced);
        puts += usize::from(put);
    }
    assert!(producers > 1000 && puts >= producers, "{producers} {puts}");
    let repro = module(Context::Actor, &method(Context::Actor, "repro", REPRO));
    assert!(
        verifier_report(&repro, Some(LocalRebindFault::PutUncarriedLocal))
            .is_some_and(|r| r.contains("ArmLocalPutNotCarried")),
        "{repro}"
    );
}
