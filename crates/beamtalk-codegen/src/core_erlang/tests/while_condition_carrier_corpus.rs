// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3999: a `whileTrue:`/`whileFalse:` whose condition sends to `self` and
//! whose body carries nothing.
//!
//! `[self done] whileTrue: [nil]` in a value-type or `TestCase` method failed
//! `erlc` with `unbound variable 'State'`. The self-send counted as a state
//! effect of the condition, which selected the loop's `StateAcc` lowering;
//! with no threaded local and no family to carry, its pack prefix seeded the
//! loop from the ambient actor `State`, which a value-type method does not
//! have. The condition's effect now counts only when the loop has a carrier
//! (`while_condition_has_carried_effects`, the condition-side twin of BT-3834's
//! `loop_body_has_carried_mutations`). The verifier catches the unbound seed as
//! `ClassMethodDefect::StateAccLoopWithoutLocals` in the `ValueType` scope.
//!
//! [`while_condition_corpus_compiles`] proves it over condition x body x
//! position x loop x method context; [`generated_while_programs_compile`] over
//! programs drawn from `arb_program`, which generates these loops. Each program
//! must pass the debug `ThreadedIr` verifier and compile through `erlc`.
//!
//! BT-4002: a condition that writes a field. A loop carries the union of the
//! families its condition and its body produce, so a value-type or `TestCase`
//! condition's `Self` is handed back by the condition closure and carried into
//! the body, the next iteration and out of the loop
//! (`VerifyError::ConditionFamilyNotCarried` checks it).
//! [`while_field_condition_corpus_compiles`] proves it over field-writing
//! condition x body x position x loop x method context. Each row agrees with
//! its body-side twin: it compiles where the same write in the body does, and
//! is otherwise the same structured rejection.
//! [`while_field_condition_corpus_reports_a_dropped_condition_write`] shows the
//! verifier catches the pre-fix lowering over the same rows.
//! [`generated_field_condition_programs_compile`] covers `arb_program`'s corpus
//! with a field write in every condition.
//! [`generated_field_condition_loops_agree_with_the_oracle_when_run`] runs
//! generated loops against a Rust model.

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Loop conditions that send to `self` (a state effect only where the loop
/// can carry it).
const CONDITIONS: &[&str] = &[
    "self done",
    "self isAlive: 0",
    "self isAlive: n",
    "(self isAlive: n) and: [true]",
    "(self isAlive: n) and: [Time nowMs < deadline]",
    "(self isAlive: n) or: [false]",
    "(self isAlive: n) ifTrue: [true] ifFalse: [false]",
    "(self take: n) > 0",
];

/// Loop bodies: carrying nothing, a self-send, an outer-local write (the
/// control: the loop threads `n`), a nested loop, a conditional whose
/// receiver is a self-send (BT-4000), `^` and a fold.
const BODIES: &[&str] = &[
    "nil",
    "Timer sleep: 0",
    "self done",
    "n := n - 1",
    "[self done] whileTrue: [nil]",
    SELF_SEND_CONDITIONAL_BODY,
    "^ 0",
    "#(1, 2) do: [:e | self isAlive: e]",
    FIELD_WRITE_BODY,
];

const SELF_SEND_CONDITIONAL_BODY: &str = "(self done) ifTrue: [Timer sleep: 0]";

/// A body that writes a field (the loop threads `SelfVt` in a value-type
/// method, the actor `State` in an actor); only where the class has `f`.
const FIELD_WRITE_BODY: &str = "self.f := self.f + 1";

/// Where the loop sits in the method body (`LOOP`).
const POSITIONS: &[&str] = &[
    "    LOOP\n",
    "    x := LOOP\n    x\n",
    "    self take: (LOOP)\n",
    "    (self isAlive: 1) ifTrue: [LOOP]\n",
    "    #(1, 2) do: [:e | LOOP]\n",
    "    1 to: 2 do: [:i | LOOP]\n",
    "    [LOOP] on: Error do: [:err | 0]\n",
];

const LOOPS: &[&str] = &["whileTrue:", "whileFalse:"];

const HELPERS: &str = "  done => false
  take: v => v
  isAlive: k => k > 0
  class done => false
  class take: v => v
  class isAlive: k => k > 0
";

/// A method context: the class header, whether methods are class-side, and
/// a statement prefixed to each method (a field write, so a value-type
/// method that threads `SelfVt` elsewhere is covered).
struct Context {
    name: &'static str,
    /// An actor instance method.
    actor_instance: bool,
    header: &'static str,
    class_side: bool,
    prelude: &'static str,
}

const CONTEXTS: &[Context] = &[
    Context {
        name: "object",
        actor_instance: false,
        header: "Object subclass: WhileCarrierProbe\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "value",
        actor_instance: false,
        header: "Value subclass: WhileCarrierProbe\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "test_case",
        actor_instance: false,
        header: "TestCase subclass: WhileCarrierProbe\n  field: f = 0\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "test_case_field_write",
        actor_instance: false,
        header: "TestCase subclass: WhileCarrierProbe\n  field: f = 0\n",
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    Context {
        name: "actor",
        actor_instance: true,
        header: "Actor subclass: WhileCarrierProbe\n  state: f = 0\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "actor_field_write",
        actor_instance: true,
        header: "Actor subclass: WhileCarrierProbe\n  state: f = 0\n",
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    Context {
        name: "object_class_side",
        actor_instance: false,
        header: "Object subclass: WhileCarrierProbe\n",
        class_side: true,
        prelude: "",
    },
    Context {
        name: "actor_class_side",
        actor_instance: false,
        header: "Actor subclass: WhileCarrierProbe\n  state: f = 0\n",
        class_side: true,
        prelude: "",
    },
];

fn passes(src: &str) -> bool {
    catch_unwind(AssertUnwindSafe(|| {
        assert_compiles_through_erlc("test", &codegen(src));
    }))
    .is_ok()
}

/// Every (name, loop source) case: condition x body x loop in statement
/// position, and condition x position x loop with each carrying-nothing body.
fn cases(ctx: &Context) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for keyword in LOOPS {
        for (c, cond) in CONDITIONS.iter().enumerate() {
            for (b, body) in BODIES.iter().enumerate() {
                let has_field = !ctx.class_side && ctx.header.contains("f = 0");
                if *body == FIELD_WRITE_BODY && !has_field {
                    continue;
                }
                out.push((
                    format!("{keyword} cond{c} body{b} statement"),
                    POSITIONS[0].replace("LOOP", &format!("[{cond}] {keyword} [{body}]")),
                ));
            }
            for (p, position) in POSITIONS.iter().enumerate().skip(1) {
                for body in &BODIES[..3] {
                    out.push((
                        format!("{keyword} cond{c} position{p} body `{body}`"),
                        position.replace("LOOP", &format!("[{cond}] {keyword} [{body}]")),
                    ));
                }
            }
        }
    }
    out
}

fn method(ctx: &Context, i: usize, body: &str) -> String {
    let side = if ctx.class_side { "class " } else { "" };
    format!(
        "  {side}probe{i} =>\n{}    n := 0\n    deadline := Time nowMs + 5\n{body}    n\n",
        ctx.prelude
    )
}

fn check_context(ctx: &Context) -> Vec<String> {
    let cases = cases(ctx);
    let methods: Vec<String> = cases
        .iter()
        .enumerate()
        .map(|(i, (_, body))| method(ctx, i, body))
        .collect();
    // One `erlc` run per context over every case at once...
    if passes(&format!("{}{HELPERS}{}", ctx.header, methods.concat())) {
        return Vec::new();
    }
    // ...and one per case only when that fails, so a failure names its case.
    cases
        .iter()
        .zip(&methods)
        .filter_map(|((name, _), method)| {
            let single = format!("{}{HELPERS}{method}", ctx.header);
            (!passes(&single)).then(|| format!("{} / {name}:\n{single}", ctx.name))
        })
        .collect()
}

#[test]
fn while_condition_corpus_compiles() {
    let failures: Vec<String> = CONTEXTS.iter().flat_map(check_context).collect();
    assert!(
        failures.is_empty(),
        "{} while-condition case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4000 (flips the `PIN-BUG BT-4000` pin): an actor loop body whose
/// statement is a conditional with a self-send receiver splices the
/// receiver's `State` `Bind` into the loop frame, so the verifier is silent.
/// The matrix is `loop_body_conditional_receiver_corpus.rs`.
#[test]
fn bt4000_actor_loop_body_self_send_conditional() {
    let src = format!(
        "Actor subclass: WhileCarrierProbe\n  state: f = 0\n{HELPERS}  probe =>\n    n := 0\n    [n > 0] whileTrue: [{SELF_SEND_CONDITIONAL_BODY}]\n    n\n"
    );
    assert_eq!(super::verifier_report(&src, None), None);
    assert!(passes(&src));
}

/// BT-4014 (flips the `PIN-BUG BT-4014` pin): a class-side while loop that
/// writes a local, as the argument of a class-side self-send, failed `erlc`
/// (an unbound `_nN`) with a silent verifier: the send's arguments were
/// compiled inside the closed `let` that pins the send's order, so the loop's
/// `LocalRebind` was scoped there. The arguments are now sequenced ahead of
/// the send; the verifier's `ProducerSiteNotThreaded` check at the method root
/// catches the pre-fix lowering
/// ([`LocalRebindFault::ClassSendOperandsInPlace`]). The matrix is
/// `class_side_operand_producer_corpus.rs`.
#[test]
fn bt4014_class_side_operand_loop_local_write() {
    let src = format!(
        "Object subclass: WhileCarrierProbe\n{HELPERS}  class probe =>\n    n := 3\n    self take: ([n > 0] whileTrue: [n := n - 1])\n    n\n"
    );
    assert_eq!(super::verifier_report(&src, None), None);
    assert!(passes(&src));
    #[cfg(debug_assertions)]
    {
        let report = super::verifier_report(&src, Some(LocalRebindFault::ClassSendOperandsInPlace));
        assert!(
            report
                .as_deref()
                .is_some_and(|r| r.contains("ProducerSiteNotThreaded")),
            "expected ProducerSiteNotThreaded for the pre-fix lowering, got {report:?}"
        );
    }
}

/// BT-4002 (flips the `PIN-BUG BT-4002` pin): a field write in a value-type
/// (here `TestCase`) loop condition is carried. Repro A (nothing else threads)
/// used to fail the verifier with `StateAccLoopWithoutLocals`; Repro B (the
/// body threads a local) compiled and looped forever, the condition's `Self1`
/// dying with its closure. Both now compile with a silent verifier and
/// through `erlc`. With the pre-fix body-only family answer
/// ([`LocalRebindFault::BodyOnlyLoopFamilies`]) the verifier reports Repro B's
/// dropped write as `ConditionFamilyNotCarried`, the check that was missing.
const REPRO_A: &str = "TestCase subclass: WhileCarrierProbe\n  field: f = 0\n  probe =>\n    [self.f := self.f + 1. self.f < 3] whileTrue: [nil]\n    self.f\n";
const REPRO_B: &str = "TestCase subclass: WhileCarrierProbe\n  field: f = 0\n  probe =>\n    k := 0\n    [self.f := self.f + 1. self.f < 3] whileTrue: [k := k + 1]\n    k\n";

#[test]
fn bt4002_value_type_condition_field_write_is_carried() {
    for src in [REPRO_A, REPRO_B] {
        assert_eq!(super::verifier_report(src, None), None, "{src}");
        assert!(passes(src), "{src}");
    }
}

#[cfg(debug_assertions)]
#[test]
fn bt4002_verifier_reports_a_condition_family_the_loop_drops() {
    for src in [REPRO_A, REPRO_B] {
        let report = super::verifier_report(src, Some(LocalRebindFault::BodyOnlyLoopFamilies));
        assert!(
            report
                .as_deref()
                .is_some_and(|r| r.contains("ConditionFamilyNotCarried")),
            "expected ConditionFamilyNotCarried for\n{src}\ngot {report:?}"
        );
    }
}

/// How many programs [`generated_while_programs_compile`] draws (256, or
/// `BT4020_PROGRAMS` for a larger seed count: the synced-calls flag-on runs use it).
fn generated_programs() -> usize {
    std::env::var("BT4020_PROGRAMS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(256)
}

/// The superclasses a generated program's `run` methods are compiled under:
/// the plain-object and value-type scopes where BT-3999 broke, the
/// `TestCase` scope it was found in, and the actor scope (the control: its
/// condition self-send threads `State`).
const GENERATED_SUPERCLASSES: &[&str] = &["Object", "Value", "TestCase", "Actor"];

#[test]
fn generated_while_programs_compile() {
    use beamtalk_core::ast::MessageSelector;
    use beamtalk_core::test_helpers::test_support::draw_programs;
    use beamtalk_core::unparse::unparse_module;

    // Every drawn program's `run` method, renamed `run<i>`, in one class.
    let mut programs = draw_programs("GenWhileProbe", generated_programs());
    let mut module = programs.remove(0);
    for (i, program) in programs.into_iter().enumerate() {
        let mut run = program.classes[0].methods[0].clone();
        run.selector = MessageSelector::Unary(format!("run{}", i + 1).into());
        module.classes[0].methods.push(run);
    }
    let source = unparse_module(&module);
    assert!(
        source.contains("] whileTrue: [") && source.contains("] whileFalse: ["),
        "the generator must produce while loops:\n{source}"
    );
    assert!(
        source.contains("[self "),
        "the generator must produce a self-send loop condition:\n{source}"
    );
    let mut failures: Vec<String> = Vec::new();
    for superclass in GENERATED_SUPERCLASSES {
        let src = source.replacen(
            "Object subclass: GenWhileProbe",
            &format!("{superclass} subclass: GenWhileProbe"),
            1,
        );
        if passes(&src) {
            continue;
        }
        // Bisect to the failing methods.
        for method in &module.classes[0].methods {
            let mut single = module.clone();
            single.classes[0].methods = vec![method.clone()];
            let src = unparse_module(&single).replacen(
                "Object subclass: GenWhileProbe",
                &format!("{superclass} subclass: GenWhileProbe"),
                1,
            );
            if !passes(&src) {
                failures.push(format!("{superclass}:\n{src}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} generated program(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

// ─── BT-4002: conditions that write a field ─────────────────────────────────

/// Loop conditions that write the field `f` (BT-4002), as top-level statements
/// of the condition block: alone, with an outer-local write before or after
/// it, twice, and ahead of a self-send or `and:` tail.
const FIELD_CONDITIONS: &[&str] = &[
    "self.f := self.f + 1. self.f < 3",
    "self.f := self.f + 1. n := n + 1. self.f < 3",
    "n := n + 1. self.f := self.f + n. self.f < 6",
    "self.f := self.f + 1. self.f := self.f + 1. self.f < 6",
    "self.f := self.f + 1. self isAlive: 3 - self.f",
    "self.f := self.f + 1. (self isAlive: self.f) and: [self.f < 3]",
];

/// Bodies crossed with [`FIELD_CONDITIONS`]: none, an outer-local write, a
/// field write, both, and a self-send.
const FIELD_CONDITION_BODIES: &[&str] = &[
    "nil",
    "n := n - 1",
    FIELD_WRITE_BODY,
    "self.f := self.f + 1. n := n + 1",
    "self done",
];

/// The body-side twin of a field-writing condition at one position: the same
/// write in the body. Where a position cannot carry the body's write (a loop
/// nested in a fold or another loop), the language rejects it with a
/// structured error, and the condition's write must be rejected the same way.
const TWIN_CONDITION: &str = "self.f < 3";

/// The method contexts with a field (or, class-side, a class variable) `f`:
/// value type, `TestCase` (with and without a prior field write), actor, and
/// class-side methods of a plain and an actor class (a class-variable write
/// is in place, ADR 0130, so nothing is threaded).
const FIELD_CONTEXTS: &[Context] = &[
    Context {
        name: "value_field",
        actor_instance: false,
        header: "Value subclass: WhileCarrierProbe\n  field: f = 0\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "value_field_write",
        actor_instance: false,
        header: "Value subclass: WhileCarrierProbe\n  field: f = 0\n",
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    Context {
        name: "test_case",
        actor_instance: false,
        header: "TestCase subclass: WhileCarrierProbe\n  field: f = 0\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "test_case_field_write",
        actor_instance: false,
        header: "TestCase subclass: WhileCarrierProbe\n  field: f = 0\n",
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    Context {
        name: "actor",
        actor_instance: true,
        header: "Actor subclass: WhileCarrierProbe\n  state: f = 0\n",
        class_side: false,
        prelude: "",
    },
    Context {
        name: "actor_field_write",
        actor_instance: true,
        header: "Actor subclass: WhileCarrierProbe\n  state: f = 0\n",
        class_side: false,
        prelude: "    self.f := 1\n",
    },
    Context {
        name: "object_class_side",
        actor_instance: false,
        header: "Object subclass: WhileCarrierProbe\n  classState: f = 0\n",
        class_side: true,
        prelude: "",
    },
    Context {
        name: "actor_class_side",
        actor_instance: false,
        header: "Actor subclass: WhileCarrierProbe\n  classState: f = 0\n",
        class_side: true,
        prelude: "",
    },
];

/// One field-condition case: its name, the loop placed at its position, and
/// the body-side twin at the same position.
struct FieldCase {
    name: String,
    source: String,
    twin: String,
}

/// Every field-writing condition x body x loop in statement position, and
/// condition x position x loop with the first three bodies.
fn field_cases() -> Vec<FieldCase> {
    let mut out = Vec::new();
    let place = |position: &str, cond: &str, keyword: &str, body: &str| {
        position.replace("LOOP", &format!("[{cond}] {keyword} [{body}]"))
    };
    for keyword in LOOPS {
        for (c, cond) in FIELD_CONDITIONS.iter().enumerate() {
            for (b, body) in FIELD_CONDITION_BODIES.iter().enumerate() {
                out.push(FieldCase {
                    name: format!("{keyword} fieldcond{c} body{b} statement"),
                    source: place(POSITIONS[0], cond, keyword, body),
                    twin: place(POSITIONS[0], TWIN_CONDITION, keyword, FIELD_WRITE_BODY),
                });
            }
            for (p, position) in POSITIONS.iter().enumerate().skip(1) {
                for body in &FIELD_CONDITION_BODIES[..3] {
                    out.push(FieldCase {
                        name: format!("{keyword} fieldcond{c} position{p} body `{body}`"),
                        source: place(position, cond, keyword, body),
                        twin: place(position, TWIN_CONDITION, keyword, FIELD_WRITE_BODY),
                    });
                }
            }
        }
    }
    out
}

/// What compiling one field-condition method came to.
#[derive(Debug, PartialEq, Eq)]
pub(super) enum FieldCodegen {
    /// Codegen succeeded (debug: with a silent verifier).
    Compiles,
    /// Codegen rejected it with a structured (non-internal) error.
    Rejected,
    /// The ADR 0131/0133 Phase 0 check rejects the program before codegen: a
    /// condition's local write that no loop threads (a class method, whose
    /// condition has no state effect, BT-3782).
    Phase0Rejected,
}

/// Compiles one method of `ctx` (debug: the verifier panics on a finding);
/// `Err` with the panic or internal error.
fn field_case_codegen(ctx: &Context, body: &str) -> Result<FieldCodegen, String> {
    codegen_outcome(&format!("{}{HELPERS}{}", ctx.header, method(ctx, 0, body)))
}

/// What compiling the module `src` comes to (debug: the verifier panics on a
/// finding); `Err` with the panic or internal error. Shared with
/// `class_side_operand_producer_corpus.rs` (BT-4014).
pub(super) fn codegen_outcome(src: &str) -> Result<FieldCodegen, String> {
    use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
    use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};
    let (module, _) =
        beamtalk_core::source_analysis::parse(beamtalk_core::source_analysis::lex_with_eof(src));
    let analysis = analyse_full(&module, AnalysisContext::default());
    if analysis.diagnostics.iter().any(|d| {
        d.severity == Severity::Error
            && matches!(
                d.category,
                Some(
                    DiagnosticCategory::UnmigratedLocalThreading
                        | DiagnosticCategory::Tier2BlockNoReturnChannel
                )
            )
    }) {
        return Ok(FieldCodegen::Phase0Rejected);
    }
    let compiled = catch_unwind(AssertUnwindSafe(|| {
        crate::core_erlang::generate_module(
            &module,
            crate::core_erlang::CodegenOptions::new("test"),
        )
    }));
    match compiled {
        Ok(Ok(_)) => Ok(FieldCodegen::Compiles),
        Ok(Err(crate::core_erlang::CodeGenError::Internal(msg))) => Err(msg),
        Ok(Err(_)) => Ok(FieldCodegen::Rejected),
        Err(payload) => Err(payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()),
    }
}

/// The field-condition rows of one context: every row codegens (verifier on)
/// or is a structured rejection exactly where its body-side twin is one, and
/// the rows that codegen compile through `erlc` (one run, bisected on failure).
fn check_field_context(ctx: &Context) -> Vec<String> {
    let mut failures = Vec::new();
    let mut compiled: Vec<(String, String)> = Vec::new();
    for case in field_cases() {
        let row = field_case_codegen(ctx, &case.source);
        let twin = field_case_codegen(ctx, &case.twin);
        match (row, twin) {
            (Err(e), _) => failures.push(format!(
                "{} / {}: broken: {e}\n{}",
                ctx.name, case.name, case.source
            )),
            // The Phase 0 check rejects outer-local writes a loop does not
            // thread (a loop's in a position no construct threads,
            // `DENY_SET`) and, in a class method, any assignment in a
            // condition block (BT-3782: a class-method condition has no state
            // effect, so its loop threads nothing out of it, class-variable
            // writes included). An instance-side row that writes only the
            // field is never one of them.
            (Ok(FieldCodegen::Phase0Rejected), _)
                if !ctx.class_side && !case.source.contains("n := ") =>
            {
                failures.push(format!(
                    "{} / {}: rejected by the Phase 0 check without a local write\n{}",
                    ctx.name, case.name, case.source
                ));
            }
            (Ok(FieldCodegen::Rejected), Ok(FieldCodegen::Compiles)) => failures.push(format!(
                "{} / {}: rejected, but the body-side twin compiles\n{}",
                ctx.name, case.name, case.source
            )),
            // The other direction: a condition write that compiles where the
            // body's is rejected is a write no loop is known to carry there.
            (Ok(FieldCodegen::Compiles), Ok(FieldCodegen::Rejected)) => failures.push(format!(
                "{} / {}: compiles, but the body-side twin is rejected\n{}",
                ctx.name, case.name, case.source
            )),
            (Ok(FieldCodegen::Compiles), _) => compiled.push((case.name, case.source)),
            (Ok(_), _) => {}
        }
    }
    // The instance-side contexts must exercise the lowering, not only the
    // rejections.
    if !ctx.class_side && compiled.len() < 100 {
        failures.push(format!(
            "{}: only {} row(s) compiled; the corpus has thinned out",
            ctx.name,
            compiled.len()
        ));
    }
    let methods: Vec<String> = compiled
        .iter()
        .enumerate()
        .map(|(i, (_, body))| method(ctx, i, body))
        .collect();
    if methods.is_empty() || passes(&format!("{}{HELPERS}{}", ctx.header, methods.concat())) {
        return failures;
    }
    failures.extend(
        compiled
            .iter()
            .zip(&methods)
            .filter_map(|((name, _), method)| {
                let single = format!("{}{HELPERS}{method}", ctx.header);
                (!passes(&single)).then(|| format!("{} / {name}: erlc:\n{single}", ctx.name))
            }),
    );
    failures
}

/// BT-4002: a field write in a `whileTrue:`/`whileFalse:` condition behaves as
/// the same write in the body, in every method context: it compiles (verifier
/// on, then `erlc`) wherever the body's would, and is otherwise the same
/// structured rejection, never a verifier finding or an `erlc` failure.
#[test]
fn while_field_condition_corpus_compiles() {
    let failures: Vec<String> = FIELD_CONTEXTS
        .iter()
        .flat_map(check_field_context)
        .collect();
    assert!(
        failures.is_empty(),
        "{} field-writing while-condition case(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// BT-4002: with the pre-fix body-only family answer
/// ([`LocalRebindFault::BodyOnlyLoopFamilies`]) every value-type row in
/// statement position reports `ConditionFamilyNotCarried`: the verifier, not
/// the repro, is what catches a dropped condition write.
#[cfg(debug_assertions)]
#[test]
fn while_field_condition_corpus_reports_a_dropped_condition_write() {
    let mut missed = Vec::new();
    for ctx in FIELD_CONTEXTS
        .iter()
        .filter(|c| !c.actor_instance && !c.class_side)
    {
        for case in field_cases()
            .into_iter()
            .filter(|c| c.name.ends_with("statement"))
        {
            let src = format!("{}{HELPERS}{}", ctx.header, method(ctx, 0, &case.source));
            let report = super::verifier_report(&src, Some(LocalRebindFault::BodyOnlyLoopFamilies));
            if !report
                .as_deref()
                .is_some_and(|r| r.contains("ConditionFamilyNotCarried"))
            {
                missed.push(format!("{} / {}: {report:?}", ctx.name, case.name));
            }
        }
    }
    assert!(
        missed.is_empty(),
        "{} row(s) where the verifier missed the dropped condition write:\n{}",
        missed.len(),
        missed.join("\n")
    );
}

/// BT-4002: [`generated_while_programs_compile`]'s corpus with a field write
/// first in every loop condition
/// (`draw_programs_with_field_writing_conditions`), under a value-type, a
/// `TestCase` and an actor class. Every method compiles with a silent
/// verifier and through `erlc`, or is a structured rejection: a loop nested in
/// a block, arm or loop body whose condition writes the field is the same
/// rejection a body-side write there is (`ValueSelfMutationLostAcrossNestedLoop`,
/// `FieldAssignmentInUnsupportedBlock`). Most must compile.
#[test]
fn generated_field_condition_programs_compile() {
    use beamtalk_core::ast::MessageSelector;
    use beamtalk_core::test_helpers::test_support::draw_programs_with_field_writing_conditions;
    use beamtalk_core::unparse::unparse_module;

    let programs =
        draw_programs_with_field_writing_conditions("GenWhileProbe", generated_programs());
    let mut module = programs[0].clone();
    module.classes[0].methods.clear();
    for (i, program) in programs.iter().enumerate() {
        let mut run = program.classes[0].methods[0].clone();
        run.selector = MessageSelector::Unary(format!("run{i}").into());
        module.classes[0].methods.push(run);
    }
    let all = unparse_module(&module);
    assert!(
        all.contains("self.f := self.f + 1"),
        "the corpus must write the field in a loop condition:\n{all}"
    );
    let mut failures = Vec::new();
    for (superclass, keyword) in [
        ("Value", "field:"),
        ("TestCase", "field:"),
        ("Actor", "state:"),
    ] {
        let ctx = |src: String| {
            src.replacen(
                "Object subclass: GenWhileProbe",
                &format!("{superclass} subclass: GenWhileProbe"),
                1,
            )
            .replacen("field: f", &format!("{keyword} f"), 1)
        };
        let mut compiled = Vec::new();
        for method in &module.classes[0].methods {
            let mut single = module.clone();
            single.classes[0].methods = vec![method.clone()];
            let src = ctx(unparse_module(&single));
            let (parsed, _) = beamtalk_core::source_analysis::parse(
                beamtalk_core::source_analysis::lex_with_eof(&src),
            );
            let result = catch_unwind(AssertUnwindSafe(|| {
                crate::core_erlang::generate_module(
                    &parsed,
                    crate::core_erlang::CodegenOptions::new("test"),
                )
            }));
            match result {
                Ok(Ok(_)) => compiled.push(method.clone()),
                Ok(Err(crate::core_erlang::CodeGenError::Internal(e))) => {
                    failures.push(format!("{superclass}: internal error {e}\n{src}"));
                }
                Ok(Err(_)) => {}
                Err(_) => failures.push(format!("{superclass}: verifier/codegen panic\n{src}")),
            }
        }
        if compiled.len() * 2 < module.classes[0].methods.len() {
            failures.push(format!(
                "{superclass}: only {} of {} generated methods compiled",
                compiled.len(),
                module.classes[0].methods.len()
            ));
        }
        let mut batch = module.clone();
        batch.classes[0].methods = compiled.clone();
        if passes(&ctx(unparse_module(&batch))) {
            continue;
        }
        for method in compiled {
            let mut single = module.clone();
            single.classes[0].methods = vec![method];
            let src = ctx(unparse_module(&single));
            if !passes(&src) {
                failures.push(format!("{superclass}: erlc\n{src}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} generated field-condition program(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// Conditions that write the field `f` somewhere other than a top-level
/// statement: in a conditional arm, an `and:` block, an assignment's or a
/// send's value, a nested loop, and a nested write after a top-level one.
const NESTED_FIELD_CONDITIONS: &[&str] = &[
    "(self.f > 0) ifTrue: [self.f := self.f - 1]. self.f < 3",
    "(n > 100) ifTrue: [self.f := 0] ifFalse: [self.f := self.f + 1]. self.f < 3",
    "(self.f < 3) and: [self.f := self.f + 1. true]",
    "(self.f := self.f + 1) < 3",
    "n := (self.f := self.f + 1). n < 3",
    "[self.f > 5] whileTrue: [self.f := self.f - 1]. self.f < 3",
    "self.f := self.f + 1. (self.f < 3) and: [self.f := self.f + 1. true]",
];

/// BT-4002: a value-type condition carries a field write only as a top-level
/// statement, as a body does. Anywhere else in the condition the write is the
/// structured rejection the body gives it
/// (`reject_unthreadable_condition_field_writes`), never a silent drop (the
/// loop never ended), a verifier panic or an `erlc` failure. In an actor the
/// write rides `State` and these compile.
#[test]
fn nested_field_writes_in_a_condition_are_rejected_like_the_body() {
    let mut failures = Vec::new();
    for ctx in FIELD_CONTEXTS.iter().filter(|c| !c.class_side) {
        for keyword in LOOPS {
            for cond in NESTED_FIELD_CONDITIONS {
                for body in &FIELD_CONDITION_BODIES[..3] {
                    let source =
                        POSITIONS[0].replace("LOOP", &format!("[{cond}] {keyword} [{body}]"));
                    let expected = if ctx.actor_instance {
                        FieldCodegen::Compiles
                    } else {
                        FieldCodegen::Rejected
                    };
                    match field_case_codegen(ctx, &source) {
                        Ok(got) if got == expected => {
                            if got == FieldCodegen::Compiles {
                                let src =
                                    format!("{}{HELPERS}{}", ctx.header, method(ctx, 0, &source));
                                if !passes(&src) {
                                    failures.push(format!("{}: erlc\n{src}", ctx.name));
                                }
                            }
                        }
                        got => failures.push(format!(
                            "{}: expected {expected:?}, got {got:?}\n{source}",
                            ctx.name
                        )),
                    }
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} nested field-condition case(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

// ─── BT-4002: execution against an oracle ───────────────────────────────────

/// A field-writing loop condition for the oracle rows. Every one strictly
/// lowers `n`, which no body writes, so each loop ends within a few iterations
/// when the write is carried (and a dropped write loops until `run_probes`'
/// timeout).
#[derive(Clone, Copy, Debug)]
enum RunCond {
    /// `self.n := self.n - 1`, then `n > 17`.
    Dec,
    /// `t := t + 1. self.n := self.n - t`, then `n > 10`.
    LocalThenField,
    /// `self.n := self.n - 1. self.m := self.m + self.n`, then `n > 17`.
    TwoFields,
}

/// A loop body for the oracle rows.
#[derive(Clone, Copy, Debug)]
enum RunBody {
    Nil,
    /// `t := t + 10`
    Local,
    /// `self.m := self.m * 2`
    Field,
    /// `self.m := self.m - 1. t := t + 100`
    Both,
}

#[derive(Clone, Copy)]
struct RunState {
    m: i64,
    n: i64,
    t: i64,
}

const RUN_START: RunState = RunState { m: 7, n: 20, t: 0 };

impl RunCond {
    const ALL: [Self; 3] = [Self::Dec, Self::LocalThenField, Self::TwoFields];

    /// The condition block's source for `whileTrue:` (`exit_when_false`) or
    /// `whileFalse:` (the negated comparison).
    fn source(self, while_true: bool) -> String {
        let (stmts, bound) = match self {
            Self::Dec => ("self.n := self.n - 1", 17),
            Self::LocalThenField => ("t := t + 1. self.n := self.n - t", 10),
            Self::TwoFields => ("self.n := self.n - 1. self.m := self.m + self.n", 17),
        };
        let test = if while_true { ">" } else { "<=" };
        format!("{stmts}. self.n {test} {bound}")
    }

    /// Runs the condition's statements; answers whether the loop continues.
    fn run(self, s: &mut RunState) -> bool {
        match self {
            Self::Dec => {
                s.n -= 1;
                s.n > 17
            }
            Self::LocalThenField => {
                s.t += 1;
                s.n -= s.t;
                s.n > 10
            }
            Self::TwoFields => {
                s.n -= 1;
                s.m += s.n;
                s.n > 17
            }
        }
    }
}

impl RunBody {
    const ALL: [Self; 4] = [Self::Nil, Self::Local, Self::Field, Self::Both];

    fn source(self) -> &'static str {
        match self {
            Self::Nil => "nil",
            Self::Local => "t := t + 10",
            Self::Field => "self.m := self.m * 2",
            Self::Both => "self.m := self.m - 1. t := t + 100",
        }
    }

    fn run(self, s: &mut RunState) {
        match self {
            Self::Nil => {}
            Self::Local => s.t += 10,
            Self::Field => s.m *= 2,
            Self::Both => {
                s.m -= 1;
                s.t += 100;
            }
        }
    }
}

/// `(probe name, method source, expected observation)` for every condition x
/// body x loop x position (statement, assigned value). The observation is
/// `#(m, n, t, r)` printed with `~w`; the loop answers `nil`.
fn run_programs() -> Vec<(String, String, String)> {
    let mut out = Vec::new();
    for while_true in [true, false] {
        let keyword = if while_true {
            "whileTrue:"
        } else {
            "whileFalse:"
        };
        for cond in RunCond::ALL {
            for body in RunBody::ALL {
                let mut s = RUN_START;
                while cond.run(&mut s) {
                    body.run(&mut s);
                }
                let expected = format!("[{},{},{},nil]", s.m, s.n, s.t);
                let lp = format!(
                    "[{}] {keyword} [{}]",
                    cond.source(while_true),
                    body.source()
                );
                for wrap in [format!("{lp}\n    r := nil"), format!("r := {lp}")] {
                    let name = format!("probe{}", out.len());
                    let method = format!(
                        "  {name} =>\n    t := 0\n    {wrap}\n    #(self.m, self.n, t, r)\n"
                    );
                    out.push((name, method, expected.clone()));
                }
            }
        }
    }
    out
}

/// BT-4002: the value-type and `TestCase` rows run. Each generated method is
/// compiled with the verifier on, through `erlc`, and run against `Self =
/// #{m => 7, n => 20}`; its `#(m, n, t, r)` must equal what the Rust model of
/// the loop computes. Before the fix a condition's write was dropped every
/// iteration, so these loops never ended.
#[test]
fn generated_field_condition_loops_agree_with_the_oracle_when_run() {
    let programs = run_programs();
    let methods: Vec<&str> = programs.iter().map(|(_, m, _)| m.as_str()).collect();
    let names: Vec<String> = programs.iter().map(|(n, _, _)| n.clone()).collect();
    for header in [
        "Value subclass: WhileCondRunProbe\n  field: n = 0\n  field: m = 0\n",
        "TestCase subclass: WhileCondRunProbe\n  field: n = 0\n  field: m = 0\n",
    ] {
        let core = codegen(&format!("{header}\n{}", methods.join("\n")));
        let self_map = format!("#{{m => {}, n => {}}}", RUN_START.m, RUN_START.n);
        let Some(observed) =
            super::value_type_fold_family_corpus::run_probes(&core, &names, &self_map)
        else {
            println!("skipping the oracle run: erlc/erl not available");
            return;
        };
        let mismatches: Vec<String> = programs
            .iter()
            .filter(|(name, _, expected)| observed.get(name) != Some(expected))
            .map(|(name, method, expected)| {
                format!(
                    "{name}: expected {expected}, ran to {:?}\n{method}",
                    observed.get(name)
                )
            })
            .collect();
        assert!(
            mismatches.is_empty(),
            "{header}: {} of {} program(s) disagree with the oracle:\n\n{}",
            mismatches.len(),
            programs.len(),
            mismatches.join("\n\n")
        );
    }
}
