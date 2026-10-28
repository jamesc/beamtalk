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
