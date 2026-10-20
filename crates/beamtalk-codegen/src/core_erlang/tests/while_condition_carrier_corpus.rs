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

use super::{assert_compiles_through_erlc, codegen};
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
/// control: the loop threads `n`), a nested loop, a conditional, `^` and a
/// fold.
///
/// PIN-BUG BT-4000: the conditional whose receiver is a self-send
/// ([`SELF_SEND_CONDITIONAL_BODY`]) is skipped in actor instance methods,
/// where it trips `UnboundVersion State1` whatever the loop condition is;
/// [`pin_bug_bt4000_actor_loop_body_self_send_conditional`] pins it.
const BODIES: &[&str] = &[
    "nil",
    "Timer sleep: 0",
    "self done",
    "n := n - 1",
    "[self done] whileTrue: [nil]",
    SELF_SEND_CONDITIONAL_BODY,
    "^ 0",
    "#(1, 2) do: [:e | self isAlive: e]",
];

const SELF_SEND_CONDITIONAL_BODY: &str = "(self done) ifTrue: [Timer sleep: 0]";

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
    /// An actor instance method (PIN-BUG BT-4000 skips one body there).
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
                if ctx.actor_instance && *body == SELF_SEND_CONDITIONAL_BODY {
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

/// PIN-BUG BT-4000: an actor loop body whose statement is a conditional with a
/// self-send receiver reads a `State1` no `Bind` produces, whatever the loop
/// condition. A fix for BT-4000 flips this pin and drops the actor skip in
/// [`cases`].
#[cfg(debug_assertions)]
#[test]
fn pin_bug_bt4000_actor_loop_body_self_send_conditional() {
    let src = format!(
        "Actor subclass: WhileCarrierProbe\n  state: f = 0\n{HELPERS}  probe =>\n    n := 0\n    [n > 0] whileTrue: [{SELF_SEND_CONDITIONAL_BODY}]\n    n\n"
    );
    let report = super::verifier_report(&src, None);
    assert!(
        report
            .as_deref()
            .is_some_and(|r| r.contains("UnboundVersion")),
        "BT-4000 pin: expected the verifier to report UnboundVersion, got {report:?}"
    );
}

/// How many programs [`generated_while_programs_compile`] draws.
const GENERATED_PROGRAMS: usize = 256;

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
    let mut programs = draw_programs("GenWhileProbe", GENERATED_PROGRAMS);
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
        source.contains("[self done] while"),
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
