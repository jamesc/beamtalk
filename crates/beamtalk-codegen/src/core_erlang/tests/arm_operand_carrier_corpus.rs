// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3949: the arm of a construct that is not a local-threading producer (a
//! conditional that writes no outer local, a `match:` arm, a `detect:ifNone:`
//! handler: `generate_plain_arm`) is a `BranchArm` frame whose statements'
//! producer sites are registered, like a producer's arm. Before the fix it
//! registered none, so an actor fold whose block self-sends (a construct that
//! writes no outer local but threads the `State` family, so it compiles to a
//! `{Value, State}` carrier) sitting at an operand site of an arm statement —
//! a map-literal value, an array element, a keyword argument — was compiled in
//! place: the consumer read the carrier tuple as the fold's value (`Tuple`
//! where `List` was expected) and the fold's `State` was dropped with it.
//!
//! `VerifyError::ConstructCarrierInPlace` fails on that shape. The tests below
//! prove it on the issue's repro (with the registration withheld,
//! `PlainArmOpaqueProducers`), over a generated corpus of every carrier
//! construct at every operand position of every plain-arm kind, and that the
//! unbroken compile of the whole corpus (actor, class and value-type methods)
//! is clean.

use super::{codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

/// The issue's repro: an actor method whose taken `ifTrue:` arm answers a map
/// literal holding a self-call `collect:`.
fn repro() -> &'static str {
    "Actor subclass: Probe
  state: flag :: Object = nil
  wrap: x => x * 10

  inBranchMapLiteral =>
    data := self.flag isNil
      ifTrue: [#{\"xs\" => (#(1, 2) asList collect: [:s | self wrap: s])}]
      ifFalse: [#{}]
    (data at: \"xs\") class
"
}

/// The Core Erlang of the dispatch clause of `selector`, the host's last
/// method, up to the dispatch fallback.
fn clause<'a>(code: &'a str, selector: &str) -> &'a str {
    let head = format!("<'{selector}'> when 'true' ->");
    let start = code.find(&head).expect("the dispatch clause");
    let rest = &code[start..];
    let end = rest.find("<OtherSelector>").unwrap_or(rest.len());
    &rest[..end]
}

#[test]
fn the_repro_unpacks_the_fold_carrier_before_the_map_literal() {
    let code = codegen(repro());
    let method = clause(&code, "inBranchMapLiteral");
    // The fold is a producer of the arm frame: its carrier is bound, the
    // `State` is read out of it, and the map's value is its element 1.
    assert!(
        method.contains("let _ThreadedResult"),
        "the fold should be lowered as a producer:\n{method}"
    );
    assert!(
        regex::Regex::new(r"=> call 'erlang':'element'\(1, _ThreadedResult@\d+\)")
            .unwrap()
            .is_match(method),
        "the map value should be the carrier's element 1:\n{method}"
    );
}

#[test]
fn the_unbroken_repro_passes_the_verifier() {
    assert_eq!(verifier_report(repro(), None), None);
}

#[test]
fn withholding_the_arm_producer_sites_is_construct_carrier_in_place() {
    let report = verifier_report(repro(), Some(LocalRebindFault::PlainArmOpaqueProducers))
        .expect("the in-place lowering is a verifier finding");
    assert!(
        report.contains("ConstructCarrierInPlace"),
        "expected ConstructCarrierInPlace, got: {report}"
    );
}

/// Actor constructs that write no outer local but thread the `State` family
/// (a self-send or a field write in the block), so each compiles to a
/// `{Value, State}` carrier.
const ACTOR_CONSTRUCTS: &[&str] = &[
    "(xs collect: [:s | self wrap: s])",
    "(xs select: [:s | (self wrap: s) > 10])",
    "(xs reject: [:s | (self wrap: s) > 10])",
    "(xs inject: 0 into: [:a :s | a + (self wrap: s)])",
    "(xs do: [:s | self wrap: s])",
    "(xs collect: [:s | self.hits := self.hits + s. s])",
    "(1 to: 2 do: [:i | self.hits := self.hits + i])",
    "([self.hits < 3] whileTrue: [self.hits := self.hits + 1])",
];

/// Constructs a class or value-type method can hold in the same positions:
/// none threads a family there, so they are the clean half of the corpus.
const PLAIN_CONSTRUCTS: &[&str] = &[
    "(xs collect: [:s | self wrap: s])",
    "(xs select: [:s | (self wrap: s) > 10])",
    "(xs inject: 0 into: [:a :s | a + (self wrap: s)])",
    "(xs do: [:s | self wrap: s])",
];

/// The arm statement `{p}` sits in, with `{c}` the construct: the operand
/// sites of a literal and of a send, as the arm's value and as a non-last
/// statement.
const POSITIONS: &[&str] = &[
    "#{\"k\" => {c}}",
    "#{\"a\" => 1, \"k\" => {c}}",
    "#[{c}]",
    "#[1, {c}]",
    "#{} at: \"k\" put: {c}",
    "({c}) size",
    "#[#{\"k\" => {c}}]",
    "r := #[{c}]. r",
    "#[{c}]. 0",
];

/// The plain arms (constructs that write no outer local, so their arms are
/// `generate_plain_arm`'s), each holding the statement `{p}`.
const ARMS: &[&str] = &[
    "flag ifTrue: [{p}] ifFalse: [nil]",
    "flag ifTrue: [nil] ifFalse: [{p}]",
    "flag ifTrue: [{p}]",
    "(flag ifTrue: [1] ifFalse: [2]) match: [1 -> {p}; _ -> nil]",
    "xs detect: [:s | s > 5] ifNone: [{p}]",
];

#[derive(Clone, Copy)]
enum Host {
    Actor,
    Class,
    Value,
}

fn program(host: Host, arm: &str, position: &str, construct: &str) -> String {
    let stmt = position.replace("{c}", construct);
    let value = arm.replace("{p}", &stmt);
    let body = format!("    xs := #(1, 2) asList\n    data := {value}\n    data\n");
    match host {
        Host::Actor => format!(
            "Actor subclass: P\n  state: hits = 0\n  wrap: x => x * 10\n  go: flag =>\n{body}"
        ),
        Host::Class => {
            format!("Object subclass: P\n  class wrap: x => x * 10\n  class go: flag =>\n{body}")
        }
        Host::Value => format!("Value subclass: P\n  wrap: x => x * 10\n  go: flag =>\n{body}"),
    }
}

fn programs(host: Host) -> Vec<String> {
    let constructs = match host {
        Host::Actor => ACTOR_CONSTRUCTS,
        Host::Class | Host::Value => PLAIN_CONSTRUCTS,
    };
    let mut out = Vec::new();
    for arm in ARMS {
        for position in POSITIONS {
            for construct in constructs {
                out.push(program(host, arm, position, construct));
            }
        }
    }
    out
}

/// How a corpus program compiles: a clean compile, a verifier finding, or a
/// shape codegen does not support at all (`UnsupportedFeature`, which no
/// verifier sees and which the fault does not change).
enum Outcome {
    Clean,
    Unsupported,
    Finding(String),
}

fn outcome(src: &str, fault: Option<LocalRebindFault>) -> Outcome {
    match verifier_report(src, fault) {
        None => Outcome::Clean,
        Some(report) if report.contains("UnsupportedFeature") => Outcome::Unsupported,
        Some(report) => Outcome::Finding(report),
    }
}

/// Every program of the corpus compiles clean, and in an actor method every
/// carrier construct in a plain arm is lowered as a producer (its carrier is
/// bound and unpacked, never embedded in the operand).
#[test]
fn every_corpus_program_compiles_clean_and_unpacks_its_carrier() {
    let mut failures = Vec::new();
    for host in [Host::Actor, Host::Class, Host::Value] {
        let mut clean = 0;
        for src in programs(host) {
            match outcome(&src, None) {
                Outcome::Clean => clean += 1,
                Outcome::Unsupported => continue,
                Outcome::Finding(report) => {
                    failures.push(format!("finding {report}\n{src}"));
                    continue;
                }
            }
            if matches!(host, Host::Actor) {
                let code = codegen(&src);
                let go = clause(&code, "go:");
                if !go.contains("let _ThreadedResult") {
                    failures.push(format!(
                        "the carrier construct should be an arm producer:\n{src}"
                    ));
                }
            }
        }
        assert!(
            clean >= 150,
            "only {clean} corpus programs compiled for this host"
        );
    }
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// With the arm's producer sites withheld, every actor program reports
/// `ConstructCarrierInPlace` and nothing else.
#[test]
fn the_fault_is_reported_over_the_generated_corpus() {
    let mut reported = 0;
    let mut missed = Vec::new();
    for src in programs(Host::Actor) {
        match outcome(&src, Some(LocalRebindFault::PlainArmOpaqueProducers)) {
            Outcome::Finding(report) => {
                assert!(
                    report.contains("ConstructCarrierInPlace"),
                    "unexpected finding {report}\n{src}"
                );
                reported += 1;
            }
            Outcome::Clean => missed.push(src),
            Outcome::Unsupported => {}
        }
    }
    assert!(
        missed.is_empty(),
        "{} programs did not report the fault:\n{}",
        missed.len(),
        missed.join("\n")
    );
    assert!(
        reported >= 300,
        "only {reported} programs reported the fault"
    );
}

// ── Value-type field-writing folds (BT-3949 review) ─────────────────────────────────────────
//
// In a value-type instance method a fold whose block writes a field threads the `Self`
// family, so it compiles to a `{Value, Self}` carrier too. A conditional arm merges `Self`
// back (the arm frame's fold producer, BT-3912), so at every operand site of an arm statement
// it must answer the fold's value with the fold's writes after the conditional; this half runs
// them on a BEAM against an oracle. A `match:` arm has no `Self` merge (`MATCH_ARM_FAMILIES`
// excludes `SelfVt`, ADR 0122), so the same fold there is rejected with the `match:`-arm
// diagnostic (`ValueSelfFieldAssignmentInMatchArm`), never compiled in place.

use super::field_assignment_value_position::VT_HEADER;
use super::value_type_fold_family_corpus::run_probes;

/// `(fold, the value it answers as `~w` prints it)`; each adds 1 + 2 to `self.n`.
const VT_FIELD_FOLDS: &[(&str, &str)] = &[
    ("(#(1, 2) collect: [:s | self.n := self.n + s. s])", "[1,2]"),
    (
        "(#(1, 2) inject: 0 into: [:a :s | self.n := self.n + s. a + s])",
        "3",
    ),
];

/// `(operand position, the value it answers given the fold's printed value)`. The keyword
/// argument is a fold's seed, compiled inline, so the probe needs no runtime dispatcher.
const VT_POSITIONS: &[(&str, &str)] = &[
    ("#(1, {c})", "[1,{v}]"),
    ("#{#k => {c}}", "#{k => {v}}"),
    ("#(9) inject: {c} into: [:a :x | a]", "{v}"),
    ("#(#(0, {c}))", "[[0,{v}]]"),
];

/// Conditional arms, taken (`self.m` starts at 0): the `{p}` arm runs. (`ifFalse:ifTrue:`
/// has no value-type `Self` merge at all, whatever the arm holds, so it is not a column here.)
const VT_CONDITIONAL_ARMS: &[&str] = &[
    "(self.m == 0) ifTrue: [{p}] ifFalse: [nil]",
    "(self.m > 0) ifTrue: [nil] ifFalse: [{p}]",
    "(self.m == 0) ifTrue: [{p}]",
];

fn vt_method(name: &str, arm: &str, position: &str, fold: &str) -> String {
    let value = arm.replace("{p}", &position.replace("{c}", fold));
    format!("  {name} =>\n    data := {value}\n    #(self.n, data)\n")
}

#[test]
fn value_type_field_writing_folds_in_conditional_arms_agree_with_the_oracle_when_run() {
    let mut methods = Vec::new();
    let mut expected = Vec::new();
    let mut rejected = Vec::new();
    for arm in VT_CONDITIONAL_ARMS {
        for (position, answer) in VT_POSITIONS {
            for (fold, value) in VT_FIELD_FOLDS {
                let name = format!("probe{}", methods.len());
                let method = vt_method(&name, arm, position, fold);
                if let Err(err) = super::compile(&format!("{VT_HEADER}\n{method}")) {
                    rejected.push(format!("{err}\n{method}"));
                    continue;
                }
                methods.push(method);
                expected.push((name, format!("[3,{}]", answer.replace("{v}", value))));
            }
        }
    }
    assert!(
        rejected.is_empty(),
        "{} rejected:\n{}",
        rejected.len(),
        rejected.join("\n")
    );
    let src = format!("{VT_HEADER}\n{}", methods.join("\n"));
    // Debug codegen: the `ThreadedIr` verifier is on and panics on any finding.
    let core = codegen(&src);
    let names: Vec<String> = expected.iter().map(|(n, _)| n.clone()).collect();
    let Some(observed) = run_probes(&core, &names, "#{m => 0, n => 0}") else {
        println!("skipping the oracle run: erlc/erl not available");
        return;
    };
    let mismatches: Vec<String> = expected
        .iter()
        .zip(&methods)
        .filter(|((name, want), _)| observed.get(name) != Some(want))
        .map(|((name, want), method)| {
            format!(
                "{name}: expected {want}, ran to {:?}\n{method}",
                observed.get(name)
            )
        })
        .collect();
    assert!(
        mismatches.is_empty(),
        "{} of {} value-type programs disagree with the oracle:\n\n{}",
        mismatches.len(),
        expected.len(),
        mismatches.join("\n")
    );
}

#[test]
fn value_type_field_writing_folds_in_match_arms_are_rejected_with_the_match_arm_diagnostic() {
    let mut checked = 0;
    for (position, _) in VT_POSITIONS {
        for (fold, _) in VT_FIELD_FOLDS {
            let arm = "self.m match: [0 -> {p}; _ -> nil]";
            let src = format!("{VT_HEADER}\n{}", vt_method("probe", arm, position, fold));
            let err = super::compile(&src).expect_err(&format!("should be rejected:\n{src}"));
            assert!(
                err.contains("inside a match: arm"),
                "expected the match:-arm diagnostic, got {err}\n{src}"
            );
            checked += 1;
        }
    }
    assert_eq!(checked, VT_POSITIONS.len() * VT_FIELD_FOLDS.len());
}

// ── Fold and loop bodies (BT-4287) ──────────────────────────────────────────────────────────
//
// The same carrier constructs at an operand site of a statement of a fold or loop body. The
// core site rule (`is_actor_fold_value_site`) leaves an actor's value-bearing sites under a
// fold frame out, because a producer that *rebinds locals* does not fit an actor fold's tuple
// accumulator (BT-3797); a construct that threads only `State` rebinds none, so codegen
// registers it there too (`is_family_only_fold_value_site`). Before BT-4287 it was compiled
// in place: `x + (self.items inject: 0 into: [...])` in a `do:` body crashed with `Tuple does
// not understand 'plusFromNumber:'`, a `collect:` answered the raw `{Value, State}` pairs, and
// a `do:` as a receiver dropped every write. `ConstructCarrierInPlace` fails on each of them
// with the registration withheld (`FoldOperandFamilyOnlyInPlace`); the right answers are run
// on a BEAM in `stdlib/test/bt4003residuals_test.bt`.

/// The inner constructs: every fold kind and counted loop, each writing no outer local but
/// threading `State` (a self-send or a field write in its block).
const FOLD_BODY_CONSTRUCTS: &[&str] = &[
    "(xs do: [:s | self wrap: s])",
    "(xs collect: [:s | self wrap: s])",
    "(xs select: [:s | (self wrap: s) > 10])",
    "(xs inject: 0 into: [:a :s | a + (self wrap: s)])",
    "(xs detect: [:s | (self wrap: s) > 10] ifNone: [0])",
    "(xs collect: [:s | self.hits := self.hits + s. s])",
    "(1 to: 2 do: [:i | self.hits := self.hits + i])",
    "(2 timesRepeat: [self wrap: 1])",
    "([self.hits < 3] whileTrue: [self.hits := self.hits + 1])",
];

/// The body statement holding the construct `{c}`: a binary and a keyword argument, a
/// receiver, a literal element, a map value, a cascade receiver, a self-send argument, an
/// operand inside an assignment's value, and the whole value of an assignment.
const FOLD_BODY_POSITIONS: &[&str] = &[
    "x + {c}",
    "0 max: {c}",
    "{c} isNil",
    "#[{c}, 1]",
    "#{#k => {c}}",
    "{c} printString; yourself",
    "self wrap: {c}",
    "q := 1 + {c}. q",
    "q := {c}. q",
];

/// Where the frame holding the statement sits relative to the fold frames around it, which
/// decides which of its value-bearing sites the core rule leaves out (`is_actor_fold_value_site`)
/// and so which cells withholding the BT-4287 registration must be reported on.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Under {
    /// The statement's own frame is a fold body: every value-bearing site is out.
    Fold,
    /// A letrec loop, an arm or an evaluated block inside a fold body: the operands are out,
    /// the whole value of an assignment is a core site.
    EnclosingFold,
    /// No fold frame encloses it: every site is a core site.
    NoFold,
    /// A frame whose registration the BT-4287 review asked to prove rather than assume (an
    /// exception body or handler, a `match:` arm, a lookup handler, an applied block, a
    /// `whileTrue:` condition): whatever the analysis accepts there must lower as a producer
    /// (`every_fold_body_operand_corpus_program_compiles_clean_and_unpacks_its_carrier`); the
    /// fault is not asserted.
    Other,
}

/// The outer body kinds holding the statement `{s}` (where `x` is bound).
const FOLD_BODY_OUTERS: &[(&str, Under)] = &[
    ("xs do: [:x | {s}]", Under::Fold),
    ("xs collect: [:x | {s}]", Under::Fold),
    ("xs inject: 0 into: [:a :x | {s}]", Under::Fold),
    ("xs select: [:x | {s}. true]", Under::Fold),
    ("xs reject: [:x | {s}. false]", Under::Fold),
    ("xs detect: [:x | {s}. false] ifNone: [0]", Under::Fold),
    (
        "xs collect: [:w | xs inject: 0 into: [:a :x | {s}]]",
        Under::Fold,
    ),
    // The outer fold also threads an outer local.
    ("xs do: [:x | t := t + x. {s}]", Under::Fold),
    ("xs do: [:x | flag ifTrue: [{s}]]", Under::EnclosingFold),
    ("xs do: [:x | [{s}] value]", Under::EnclosingFold),
    ("xs do: [:x | 2 timesRepeat: [{s}]]", Under::EnclosingFold),
    ("1 to: 2 do: [:x | {s}]", Under::NoFold),
    ("2 timesRepeat: [x := 1. {s}]", Under::NoFold),
    ("[self.hits > 100] whileTrue: [x := 1. {s}]", Under::NoFold),
    ("xs do: [:x | [{s}] on: Error do: [:e | 0]]", Under::Other),
    ("xs do: [:x | [0] on: Error do: [:e | {s}]]", Under::Other),
    ("xs do: [:x | [{s}] ensure: [0]]", Under::Other),
    ("xs do: [:x | [0] ensure: [{s}]]", Under::Other),
    ("xs do: [:x | x match: [1 -> {s}; _ -> 0]]", Under::Other),
    ("xs do: [:x | #{} at: x ifAbsent: [{s}]]", Under::Other),
    (
        "xs detect: [:w | w > 5] ifNone: [x := 1. {s}]",
        Under::Other,
    ),
    ("xs do: [:w | [:x | {s}] value: w]", Under::Other),
    (
        "xs do: [:w | [x := w. {s}. false] whileTrue: [0]]",
        Under::Other,
    ),
    ("[x := 1. {s}] on: Error do: [:e | 0]", Under::Other),
    ("[x := 1. {s}] ensure: [0]", Under::Other),
    ("1 match: [1 -> [:x | {s}] value: 1; _ -> 0]", Under::Other),
    ("#{} at: 1 ifAbsent: [x := 1. {s}]", Under::Other),
    // Producer arms (the construct writes the outer local `t`), a stored closure (Tier 2:
    // it sends to `self`) and an applied block in a closure.
    ("xs do: [:x | flag ifTrue: [t := t + 1. {s}]]", Under::Other),
    (
        "xs do: [:x | [t := t + 1. {s}] on: Error do: [:e | 0]]",
        Under::Other,
    ),
    ("xs do: [:x | [0] ensure: [t := t + 1. {s}]]", Under::Other),
    ("xs do: [:w | b := [:x | {s}]. b value: w]", Under::Other),
    ("[:w | xs do: [:x | {s}]] value: 1", Under::Other),
];

const FOLD_BODY_HEADER: &str = "Actor subclass: P\n  state: hits = 0\n  wrap: x => x * 10\n";

/// The method `selector` holding the cell: the outer body, with the statement holding the
/// construct.
fn fold_body_method(selector: &str, outer: &str, position: &str, construct: &str) -> String {
    let stmt = position.replace("{c}", construct);
    let value = outer.replace("{s}", &stmt);
    format!(
        "  {selector} flag =>\n    t := 0\n    xs := #(1, 2) asList\n    r := {value}\n    #[r, t]\n"
    )
}

/// One corpus cell: its `go:` method, and whether withholding the BT-4287 registration must
/// be reported on it.
struct FoldBodyCell {
    outer: usize,
    method: String,
    must_report: bool,
}

impl FoldBodyCell {
    fn program(&self) -> String {
        format!("{FOLD_BODY_HEADER}{}", self.method)
    }
}

fn fold_body_cells() -> Vec<FoldBodyCell> {
    let mut out = Vec::new();
    for (outer_index, (outer, under)) in FOLD_BODY_OUTERS.iter().enumerate() {
        for position in FOLD_BODY_POSITIONS {
            // A `match:` arm holds an expression, not statements or a cascade.
            if outer.contains("->") && (position.contains(". ") || position.contains(';')) {
                continue;
            }
            for construct in FOLD_BODY_CONSTRUCTS {
                // A cascade is not an operand site at all: its own lowering sequences its
                // receiver (`cascade_operand_state_corpus`), so it is in the clean half only.
                let whole_value = position.starts_with("q := {c}");
                let cascade = position.contains(';');
                let must_report = !cascade
                    && match under {
                        Under::Fold => true,
                        Under::EnclosingFold => !whole_value,
                        Under::NoFold | Under::Other => false,
                    };
                out.push(FoldBodyCell {
                    outer: outer_index,
                    method: fold_body_method("go:", outer, position, construct),
                    must_report,
                });
            }
        }
    }
    out
}

/// Every cell compiles clean through the verifier, its construct is lowered as a producer
/// (its carrier bound and unpacked, never embedded in the operand), and every cell of each
/// outer body kind, as the methods of one class, compiles through `erlc`.
#[test]
fn every_fold_body_operand_corpus_program_compiles_clean_and_unpacks_its_carrier() {
    let mut failures = Vec::new();
    let mut clean = 0;
    let mut per_outer: Vec<Vec<String>> = vec![Vec::new(); FOLD_BODY_OUTERS.len()];
    for cell in fold_body_cells() {
        let src = cell.program();
        // BT-4310: the analysis accepts every cell (the `StateFoldUnsequencedOperand` row
        // is gone), so every one reaches codegen.
        if analysis_rejects(&src) {
            failures.push(format!("the analysis should accept:\n{src}"));
            continue;
        }
        match outcome(&src, None) {
            Outcome::Clean => clean += 1,
            Outcome::Unsupported => continue,
            Outcome::Finding(report) => {
                failures.push(format!("finding {report}\n{src}"));
                continue;
            }
        }
        // Lowered as a producer (its carrier bound and unpacked), or not compiled to a
        // carrier at all: a block that is a real closure (the `ifAbsent:` handler of a
        // lookup that is not inlined) runs its self-sends against the actor-state cell
        // (ADR 0137), so its fold threads no `StateAcc`.
        let code = codegen(&src);
        let go = clause(&code, "go:");
        if !go.contains("let _ThreadedResult") && go.contains("StateAcc") {
            failures.push(format!(
                "the carrier construct should be a producer:\n{src}"
            ));
            continue;
        }
        let methods = &mut per_outer[cell.outer];
        let selector = format!("go{}:", methods.len());
        methods.push(cell.method.replacen("go:", &selector, 1));
    }
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n")
    );
    assert!(clean >= 1000, "only {clean} corpus programs compiled clean");
    println!("{clean} lowered");
    for methods in per_outer.into_iter().filter(|m| !m.is_empty()) {
        let src = format!("{FOLD_BODY_HEADER}{}", methods.join(""));
        super::assert_compiles_through_erlc("test", &codegen(&src));
    }
}

/// With the BT-4287 sites withheld, every cell under a fold frame reports
/// `ConstructCarrierInPlace`, and no cell reports anything else.
#[test]
fn withholding_the_fold_body_family_only_sites_is_reported_over_the_corpus() {
    let mut reported = 0;
    let mut missed = Vec::new();
    for cell in fold_body_cells() {
        let src = cell.program();
        match outcome(&src, Some(LocalRebindFault::FoldOperandFamilyOnlyInPlace)) {
            Outcome::Finding(report) => {
                assert!(
                    report.contains("ConstructCarrierInPlace"),
                    "unexpected finding {report}\n{src}"
                );
                reported += 1;
            }
            Outcome::Clean if cell.must_report => missed.push(src),
            Outcome::Clean | Outcome::Unsupported => {}
        }
    }
    assert!(
        missed.is_empty(),
        "{} programs did not report the fault:\n{}",
        missed.len(),
        missed.join("\n")
    );
    assert!(
        reported >= 690,
        "only {reported} programs reported the fault"
    );
}

// ── Operands the sequencing rule leaves to another lowering (BT-4310) ──────────────────────
//
// The site walk (`operand_sites_of_statement`) stops at operands another lowering compiles
// itself: a conditional's receiver, a lookup key, a cascade message's argument, the arguments
// of a `super`, `error:`, block-invocation or block-taking send. A carrier construct there was
// compiled in place (measured on a BEAM at the method root and in a fold body: a crash, the
// raw carrier or dropped writes), and the analysis rejected it (`StateFoldUnsequencedOperand`).
// BT-4310 registers it as a producer site of the frame that lowers its statement, the method
// root included (`family_only_operand_site_exprs`), and each of those lowerings sequences the
// operand through `threaded_expression`. Proved here over every outer body of the fold-body
// corpus and the method root, in both directions: every cell is accepted, compiles clean and
// lowers its construct as a producer of the frame (or, in a real closure, compiles no carrier
// at all: it runs against the actor-state cell, ADR 0137); and with the registration withheld
// (`UnsequencedOperandInPlace`) every cell whose construct was a producer reports
// `ConstructCarrierInPlace`, except in an `on:do:`/`ensure:` frame, which checks no site. The
// right answers are run on a BEAM in `stdlib/test/bt4003residuals_test.bt`. (A cast is a
// statement only, so it cannot be a fold body's value; its cells are in `bt4310_*` in
// `semantic_analysis/tests` and on the BEAM. A `match:` guard allows only comparisons and
// arithmetic, so it cannot hold one at all.)

use crate::core_erlang::threaded_expr::fault::recording_producers;
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};

/// The statement holding the construct `{c}` at an operand the sequencing rule leaves to
/// another lowering.
const UNSEQUENCED_POSITIONS: &[&str] = &[
    "({c} isNil) ifTrue: [1] ifFalse: [0]",
    "(0 < {c} size) ifTrue: [1]",
    "{c} isNil and: [true]",
    "{c} isNil or: [false]",
    "{c} ifNil: [0] ifNotNil: [:v | v]",
    "{c} isNil ifFalse: [1]",
    "#{} at: {c} ifAbsent: [0]",
    "#{} at: {c} ifPresent: [:v | v] ifAbsent: [0]",
    "super printString: {c}",
    "self error: {c} printString",
    "[:z | z] value: {c}",
    "self apply: {c} with: [:z | z]",
    "xs size; max: {c} size",
    "self wrap: 1; wrap: {c} size",
];

/// The outer bodies of [`FOLD_BODY_OUTERS`] whose statement is in a real closure: a lookup
/// handler that writes no outer local (the lookup is not inlined; `x` is the handler's own
/// local) and a stored block.
const CLOSURE_OUTERS: &[&str] = &[
    "xs do: [:x | #{} at: x ifAbsent: [{s}]]",
    "#{} at: 1 ifAbsent: [x := 1. {s}]",
    "xs do: [:w | b := [:x | {s}]. b value: w]",
];

/// Whether the analysis rejects `src` with a local-threading error.
fn analysis_rejects(src: &str) -> bool {
    let (module, parse_diags) = super::parse_normalised(src);
    assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
    analyse_full(&module, AnalysisContext::default())
        .diagnostics
        .iter()
        .any(|d| {
            d.severity == beamtalk_core::source_analysis::Severity::Error
                && d.category
                    == Some(beamtalk_core::source_analysis::DiagnosticCategory::UnmigratedLocalThreading)
        })
}

/// One BT-4310 cell: its program, the outer body it sits in (`None` at the method root),
/// and the byte range of its construct in the program.
struct UnsequencedCell {
    outer: Option<usize>,
    src: String,
    construct: std::ops::Range<usize>,
}

impl UnsequencedCell {
    /// Whether the cell's frame is an `on:do:`/`ensure:` body or handler, which checks no
    /// producer site (`verify_producer_sites_threaded` is not run there).
    fn in_exception_frame(&self) -> bool {
        self.outer.is_some_and(|o| {
            let outer = FOLD_BODY_OUTERS[o].0;
            outer.contains(" on: Error do: ") || outer.contains(" ensure: ")
        })
    }

    /// Whether the cell's statement is in a real closure (a block that is stored, or the
    /// handler of a lookup that is not inlined), which compiles no carrier.
    fn in_real_closure(&self) -> bool {
        self.outer
            .is_some_and(|o| CLOSURE_OUTERS.contains(&FOLD_BODY_OUTERS[o].0))
    }

    /// Whether the codegen of the cell lowered its construct as a producer.
    fn construct_is_producer(
        &self,
        records: &[(beamtalk_core::source_analysis::Span, Vec<String>)],
    ) -> bool {
        records.iter().any(|(span, _)| {
            span.start() as usize >= self.construct.start
                && span.end() as usize <= self.construct.end
        })
    }
}

fn unsequenced_cells() -> Vec<UnsequencedCell> {
    let mut outers: Vec<(Option<usize>, &str)> = FOLD_BODY_OUTERS
        .iter()
        .enumerate()
        .map(|(i, (o, _))| (Some(i), *o))
        .collect();
    // The method root holds the statement itself.
    outers.push((None, "{s}"));
    let mut out = Vec::new();
    for (outer_index, outer) in outers {
        for position in UNSEQUENCED_POSITIONS {
            // A `match:` arm holds an expression, not a cascade.
            if outer.contains("->") && position.contains(';') {
                continue;
            }
            for construct in FOLD_BODY_CONSTRUCTS {
                let src = format!(
                    "{FOLD_BODY_HEADER}{}",
                    fold_body_method("go:", outer, position, construct)
                );
                let start = src
                    .find(construct)
                    .expect("the construct is in the program");
                out.push(UnsequencedCell {
                    outer: outer_index,
                    src,
                    construct: start..start + construct.len(),
                });
            }
        }
    }
    // The statement itself at the method root, not an assignment's value: each statement
    // kind's own lowering (a `super` or `error:` send, a cascade, a conditional) holds it.
    for position in UNSEQUENCED_POSITIONS {
        for construct in FOLD_BODY_CONSTRUCTS {
            let stmt = position.replace("{c}", construct);
            let src = format!(
                "{FOLD_BODY_HEADER}  go: flag =>\n    t := 0\n    xs := #(1, 2) asList\n    {stmt}\n    #[0, t]\n"
            );
            let start = src
                .find(construct)
                .expect("the construct is in the program");
            out.push(UnsequencedCell {
                outer: None,
                src,
                construct: start..start + construct.len(),
            });
        }
    }
    out
}

/// Every cell is accepted, compiles clean, and lowers its construct as a producer of the
/// frame, or (in a real closure) compiles no carrier for it; every cell of each outer body
/// kind, as the methods of one class, compiles through `erlc`.
#[test]
fn every_unsequenced_operand_cell_is_lowered_as_a_producer() {
    let mut failures = Vec::new();
    let mut lowered = 0;
    let mut cell_frames = 0;
    let mut per_outer: std::collections::BTreeMap<Option<usize>, Vec<String>> =
        std::collections::BTreeMap::new();
    let cells = unsequenced_cells();
    for cell in &cells {
        if analysis_rejects(&cell.src) {
            failures.push(format!("the analysis should accept:\n{}", cell.src));
            continue;
        }
        match outcome(&cell.src, None) {
            Outcome::Clean => {}
            Outcome::Unsupported => {
                failures.push(format!("unsupported:\n{}", cell.src));
                continue;
            }
            Outcome::Finding(report) => {
                failures.push(format!("finding {report}\n{}", cell.src));
                continue;
            }
        }
        let (_, records) = recording_producers(|| codegen(&cell.src));
        match (cell.construct_is_producer(&records), cell.in_real_closure()) {
            (true, false) => lowered += 1,
            // A real closure compiles no carrier: its fold runs against the actor-state
            // cell (ADR 0137), so it is no producer there.
            (false, true) => cell_frames += 1,
            (true, true) => {
                failures.push(format!(
                    "a closure's construct is no producer:\n{}",
                    cell.src
                ));
                continue;
            }
            (false, false) => {
                failures.push(format!(
                    "the carrier construct should be a producer:\n{}",
                    cell.src
                ));
                continue;
            }
        }
        let methods = per_outer.entry(cell.outer).or_default();
        let selector = format!("go{}:", methods.len());
        let method = cell.src[FOLD_BODY_HEADER.len()..].replacen("go:", &selector, 1);
        methods.push(method);
    }
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n")
    );
    println!(
        "{} cells: {lowered} lowered as producers, {cell_frames} compiled no carrier",
        cells.len()
    );
    assert!(lowered >= 2500, "only {lowered} cells lowered as producers");
    for methods in per_outer.into_values() {
        let src = format!("{FOLD_BODY_HEADER}{}", methods.join(""));
        super::assert_compiles_through_erlc("test", &codegen(&src));
    }
}

/// With the BT-4310 sites withheld, every cell whose construct the unbroken compile lowers
/// as a producer reports `ConstructCarrierInPlace` (outside an exception frame), and no cell
/// reports anything else.
#[test]
fn withholding_the_unsequenced_operand_sites_is_reported_over_the_corpus() {
    let mut reported = 0;
    let mut missed = Vec::new();
    for cell in unsequenced_cells() {
        let (_, records) = recording_producers(|| codegen(&cell.src));
        let must_report = cell.construct_is_producer(&records) && !cell.in_exception_frame();
        match outcome(&cell.src, Some(LocalRebindFault::UnsequencedOperandInPlace)) {
            Outcome::Finding(report) => {
                assert!(
                    report.contains("ConstructCarrierInPlace"),
                    "unexpected finding {report}\n{}",
                    cell.src
                );
                reported += 1;
            }
            Outcome::Clean if must_report => missed.push(cell.src),
            Outcome::Clean | Outcome::Unsupported => {}
        }
    }
    assert!(
        missed.is_empty(),
        "{} programs did not report the fault:\n{}",
        missed.len(),
        missed.join("\n")
    );
    assert!(
        reported >= 2000,
        "only {reported} programs reported the fault"
    );
}

/// The issue's repros (measured on a BEAM before BT-4310: a crash, a wrong value, the raw
/// carrier or dropped writes), at the method root and in a `collect:` body: lowered as a
/// producer now, and `ConstructCarrierInPlace` with the BT-4310 sites withheld.
#[test]
fn the_bt4310_repros_are_producers_and_withholding_their_sites_is_reported() {
    let fold = "(self.items inject: 0 into: [:b :y | b + self bump])";
    let header = "Actor subclass: P\n  state: n = 0\n  state: items = #(1, 2)\n  \
                  bump =>\n    self.n := self.n + 1\n    self.n\n  \
                  apply: v with: blk => blk value: v\n";
    let positions = [
        "({f} > 3) ifTrue: [1] ifFalse: [0]",
        "(0 < {f}) ifTrue: [1] ifFalse: [0]",
        "({f} > 3) and: [true]",
        "({f} > 3) or: [false]",
        "{f} ifNil: [0] ifNotNil: [:v | v + 1]",
        "#{3 => 30} at: {f} ifAbsent: [0]",
        "super printString: {f}",
        "self error: {f} printString",
        "[:z | z + 1] value: {f}",
        "self apply: {f} with: [:z | z]",
        "0 max: 1; max: {f}",
    ];
    for position in positions {
        let value = position.replace("{f}", fold);
        for body in [
            format!("    r := {value}\n    r\n"),
            format!("    r := #(1, 2) collect: [:x | {value}]\n    r\n"),
        ] {
            let src = format!("{header}  go =>\n{body}");
            let (_, records) = recording_producers(|| codegen(&src));
            let start = src.find(fold).expect("the fold");
            assert!(
                records
                    .iter()
                    .any(|(span, _)| span.start() as usize >= start
                        && span.end() as usize <= start + fold.len()),
                "the fold should be a producer:\n{src}"
            );
            let report = verifier_report(&src, Some(LocalRebindFault::UnsequencedOperandInPlace))
                .unwrap_or_else(|| panic!("the in-place lowering should be reported:\n{src}"));
            assert!(
                report.contains("ConstructCarrierInPlace"),
                "expected ConstructCarrierInPlace, got {report}\n{src}"
            );
        }
    }
}
