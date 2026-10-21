// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4014: a local-threading producer in an operand of a class-side self-send.
//!
//! `self take: ([n > 0] whileTrue: [n := n - 1])` in a class method failed `erlc`
//! with an unbound `_nN`, the verifier silent. A class-side self-send (like a
//! class-variable write) is bound in an ordered `let` of its own to pin its order
//! against its siblings (`class_method_prelude_producer`, ADR 0130). Only a
//! write's value was sequenced ahead of that `let`; a send's arguments were
//! compiled inside it, so a loop, fold, conditional or exception construct there
//! rebound its locals inside the `let`'s value and the next statement read an
//! unbound version.
//!
//! **Invariant** (every method-root scope: a class method, a value-type method,
//! and the plain-object and `TestCase` instance methods compiled as value types;
//! the arm, loop, fold, closure and Tier 2 frames check their own statements the
//! same way): a producer at a sequenced operand site of a statement has its
//! `ConstructTuple`, threading every local it writes, at the top level of the
//! prelude its statement splices, so its `LocalRebind`s are in scope for every
//! later statement. `lower_root_statement_with_producer` checks it as
//! `VerifyError::ProducerSiteNotThreaded`, the BT-3939 rule.
//!
//! [`class_side_operand_producers_compile`] proves it over producer x operand
//! position x method context: each row compiles with a silent verifier and
//! through `erlc`, or is a structured rejection exactly where its instance-side
//! twin (the same method in a plain `Object` instance method) is one.
//! [`class_side_operand_producers_report_the_pre_fix_lowering`] shows the
//! verifier catches the pre-fix lowering over the same rows.

use super::while_condition_carrier_corpus::{FieldCodegen, codegen_outcome};
use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Constructs that write the outer local `n` (`PRODUCER`): every loop, fold,
/// conditional and exception family, and a value-position assignment.
const PRODUCERS: &[&str] = &[
    "[n > 0] whileTrue: [n := n - 1]",
    "[n <= 0] whileFalse: [n := n - 1]",
    "1 to: 3 do: [:i | n := n + i]",
    "3 timesRepeat: [n := n + 1]",
    "#(1, 2) do: [:e | n := n + e]",
    "#(1, 2) collect: [:e | n := n + e]",
    "#(1, 2) inject: 0 into: [:a :e | n := n + e. a + e]",
    "n > 0 ifTrue: [n := 0] ifFalse: [n := 1]",
    "[n := n + 1] on: Error do: [:err | n := 0]",
    "[n := n + 1] ensure: [n := n + 2]",
    "n := n + 1",
];

/// Where the producer sits (`PROD`): the arguments of a self-send of every arity
/// and nesting, around a sibling that reads `n`, inside a binary operand, as an
/// assignment's value, as a class-variable write's value (class-side only),
/// as an argument of a send to the class by name, and in an arm and a loop body
/// frame of its own (followed by a read of `n` in that frame). (A class-side closure that
/// writes an outer local is a structured rejection, BT-3743.)
const POSITIONS: &[&str] = &[
    "    self take: (PROD)\n",
    "    self take: 1 with: (PROD)\n",
    "    self take: (PROD) with: n\n",
    "    self take: n with: (PROD)\n",
    "    self take: (PROD) with: (self take: 2)\n",
    "    self take: (self take: (PROD))\n",
    "    self take: (PROD) + 1\n",
    "    self take: ((PROD) printString)\n",
    "    x := self take: (PROD)\n    x\n",
    "    self take: (self take: 1) with: (PROD)\n",
    "    OperandProducerProbe take: (PROD)\n",
    "    (self isAlive: 1) ifTrue: [self take: (PROD)]\n",
    "    (self isAlive: 1) ifTrue: [self take: (PROD). self take: n] ifFalse: [0]\n",
    "    #(1, 2) do: [:e | self take: (PROD)]\n",
    "    #(1, 2) do: [:e | self take: (PROD). self take: n]\n",
    "    (self take: (PROD)) printString\n",
];

/// Class-variable writes of the class-side contexts (`classState: f`).
const CLASS_VAR_POSITIONS: &[&str] = &[
    "    self.f := self take: (PROD)\n",
    "    self.f := (self take: (PROD)) + self.f\n",
];

const HELPERS: &str = "  take: v => v
  take: a with: b => a
  isAlive: k => k > 0
  class take: v => v
  class take: a with: b => a
  class isAlive: k => k > 0
";

/// A method context: the class header and whether the methods are class-side.
struct Context {
    name: &'static str,
    header: &'static str,
    class_side: bool,
}

const CONTEXTS: &[Context] = &[
    Context {
        name: "object_class_side",
        header: "Object subclass: OperandProducerProbe\n  classState: f = 0\n",
        class_side: true,
    },
    Context {
        name: "actor_class_side",
        header: "Actor subclass: OperandProducerProbe\n  state: g = 0\n  classState: f = 0\n",
        class_side: true,
    },
    Context {
        name: "value_class_side",
        header: "Value subclass: OperandProducerProbe\n  field: g = 0\n  classState: f = 0\n",
        class_side: true,
    },
    Context {
        name: "stateless_class_side",
        header: "Object subclass: OperandProducerProbe\n",
        class_side: true,
    },
    // The instance-side controls: these lower a self-send as an ordinary send.
    Context {
        name: "object",
        header: "Object subclass: OperandProducerProbe\n",
        class_side: false,
    },
    Context {
        name: "value",
        header: "Value subclass: OperandProducerProbe\n  field: g = 0\n",
        class_side: false,
    },
    Context {
        name: "test_case",
        header: "TestCase subclass: OperandProducerProbe\n",
        class_side: false,
    },
    Context {
        name: "actor",
        header: "Actor subclass: OperandProducerProbe\n  state: g = 0\n",
        class_side: false,
    },
];

/// Every (name, statement) row of `ctx`: producer x position.
fn rows(ctx: &Context) -> Vec<(String, String)> {
    let class_var = ctx.class_side && ctx.header.contains("classState: f");
    let positions = POSITIONS
        .iter()
        .chain(CLASS_VAR_POSITIONS.iter().filter(|_| class_var));
    let mut out = Vec::new();
    for (p, position) in positions.enumerate() {
        for (k, producer) in PRODUCERS.iter().enumerate() {
            out.push((
                format!("producer{k} position{p}"),
                position.replace("PROD", producer),
            ));
        }
    }
    out
}

/// One method of `ctx` holding `stmt`, reading `n` after it.
fn method(ctx: &Context, i: usize, stmt: &str) -> String {
    let side = if ctx.class_side { "class " } else { "" };
    format!("  {side}probe{i} =>\n    n := 3\n{stmt}    n\n")
}

fn module(ctx: &Context, methods: &str) -> String {
    format!("{}{HELPERS}{methods}", ctx.header)
}

fn passes(src: &str) -> bool {
    catch_unwind(AssertUnwindSafe(|| {
        assert_compiles_through_erlc("test", &codegen(src));
    }))
    .is_ok()
}

/// The instance-side twin of a row: the same statement in a plain `Object`
/// instance method.
fn twin_outcome(stmt: &str) -> Result<FieldCodegen, String> {
    codegen_outcome(&module(&CONTEXTS[4], &method(&CONTEXTS[4], 0, stmt)))
}

/// The fold producers of [`PRODUCERS`] (`do:`, `collect:`, `inject:into:`).
const FOLD_PRODUCERS: std::ops::Range<usize> = 4..7;

/// PIN-BUG BT-4089: in an actor class method, a fold that writes a local in
/// value position (any of [`POSITIONS`]) repacks its locals into the ambient
/// actor `State`, which a class method does not have: `erlc` rejects it
/// (`unbound variable 'State'`), the verifier silent. The rows are skipped;
/// [`pin_bug_bt4089_actor_class_side_fold_in_value_position`] pins it. A fix
/// for BT-4089 flips the pin and drops this skip.
fn pinned_bt4089(ctx: &Context, stmt: &str) -> bool {
    ctx.name == "actor_class_side"
        && PRODUCERS[FOLD_PRODUCERS]
            .iter()
            .any(|producer| stmt.contains(producer))
}

#[test]
fn pin_bug_bt4089_actor_class_side_fold_in_value_position() {
    if !super::erlc_available() {
        return;
    }
    let ctx = &CONTEXTS[1];
    let src = module(
        ctx,
        &method(ctx, 0, "    x := #(1, 2) collect: [:e | n := n + e]\n"),
    );
    assert_eq!(super::verifier_report(&src, None), None);
    assert!(
        !passes(&src),
        "BT-4089 pin: expected erlc to reject the actor class-side fold"
    );
}

fn check_context(ctx: &Context) -> Vec<String> {
    let mut failures = Vec::new();
    let mut compiled: Vec<(String, String)> = Vec::new();
    for (name, stmt) in rows(ctx) {
        if pinned_bt4089(ctx, &stmt) {
            continue;
        }
        let src = module(ctx, &method(ctx, 0, &stmt));
        match (codegen_outcome(&src), twin_outcome(&stmt)) {
            (Err(e), _) => failures.push(format!("{} / {name}: broken: {e}\n{src}", ctx.name)),
            (Ok(FieldCodegen::Compiles), _) => compiled.push((name, stmt)),
            // A rejection (structured or the Phase 0 check) where the
            // instance-side twin compiles is a class-side gap.
            (Ok(_), Ok(FieldCodegen::Compiles)) if ctx.class_side => failures.push(format!(
                "{} / {name}: rejected, but the instance-side twin compiles\n{src}",
                ctx.name
            )),
            (Ok(_), _) => {}
        }
    }
    if compiled.len() < 100 {
        failures.push(format!(
            "{}: only {} row(s) compiled; the corpus has thinned out",
            ctx.name,
            compiled.len()
        ));
    }
    let methods: Vec<String> = compiled
        .iter()
        .enumerate()
        .map(|(i, (_, stmt))| method(ctx, i, stmt))
        .collect();
    // One `erlc` run per context over every row at once, and one per row only
    // when that fails, so a failure names its row.
    if methods.is_empty() || passes(&module(ctx, &methods.concat())) {
        return failures;
    }
    failures.extend(compiled.iter().zip(&methods).filter_map(|((name, _), m)| {
        let single = module(ctx, m);
        (!passes(&single)).then(|| format!("{} / {name}: erlc:\n{single}", ctx.name))
    }));
    failures
}

#[test]
fn class_side_operand_producers_compile() {
    let failures: Vec<String> = CONTEXTS.iter().flat_map(check_context).collect();
    assert!(
        failures.is_empty(),
        "{} operand-producer row(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4014: with the pre-fix lowering
/// ([`LocalRebindFault::ClassSendOperandsInPlace`]: a class-side self-send's
/// arguments compiled inside its closed `let`) every class-side row whose
/// producer is a direct argument of a method-root self-send statement reports
/// `ProducerSiteNotThreaded`: the verifier, not `erlc`, is what catches it.
#[cfg(debug_assertions)]
#[test]
fn class_side_operand_producers_report_the_pre_fix_lowering() {
    // The positions whose producer is a direct argument of the statement's own
    // self-send (the shape the fault reverts).
    const DIRECT: &[usize] = &[0, 1, 2, 3, 4, 9];
    let mut missed = Vec::new();
    let mut checked = 0;
    for ctx in CONTEXTS.iter().filter(|c| c.class_side) {
        for &p in DIRECT {
            for producer in PRODUCERS {
                let stmt = POSITIONS[p].replace("PROD", producer);
                if pinned_bt4089(ctx, &stmt) {
                    continue;
                }
                let src = module(ctx, &method(ctx, 0, &stmt));
                if codegen_outcome(&src) != Ok(FieldCodegen::Compiles) {
                    continue;
                }
                checked += 1;
                let report =
                    super::verifier_report(&src, Some(LocalRebindFault::ClassSendOperandsInPlace));
                if !report
                    .as_deref()
                    .is_some_and(|r| r.contains("ProducerSiteNotThreaded"))
                {
                    missed.push(format!("{} / {stmt}: {report:?}", ctx.name));
                }
            }
        }
    }
    assert!(checked >= 100, "only {checked} row(s) checked");
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the pre-fix lowering:\n{}",
        missed.len(),
        missed.join("\n")
    );
}
