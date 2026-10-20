// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3939: a Tier 2 block body (a block literal an actor self-send promotes,
//! `detect_tier2_self_send`) is an arm frame (`BranchArm`, ADR 0131 §2) whose
//! loop and fold statements are local-threading producers. Before the fix the
//! body registered no producer sites, so `xs do: [:y | n := n + y]` was lowered
//! inside an opaque `let _ = ( ... )`: the fold's rebind of the block-local `n`
//! and the state it handed back were scoped inside that statement, and the block
//! answered the pre-loop `n` with the pre-loop state (`#(0, 0)`, not `#(3, 3)`).
//!
//! `VerifyError::ProducerSiteNotThreaded` fails on that shape. The tests below
//! prove it three ways: on the issue's repro (with the registration withheld,
//! `Tier2OpaqueProducers`), on a generated corpus of every loop or fold
//! construct at every operand position of a promoted block, and that the
//! unbroken compile of the whole corpus is clean.

use super::{codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

const REPRO: &str = "Actor subclass: P
  state: groups = #(#(1, 2), #(3))
  state: hits = 0
  sealed fwd: block => self.groups collect: block
  promote => self fwd: [:xs | self.hits := self.hits + 1. xs]
  sums => self fwd: [:xs | n := 0. xs do: [:y | n := n + y]. n]
";

/// The method `go`'s Core Erlang, from its dispatch clause to the next one.
fn go_clause(code: &str) -> &str {
    let start = code
        .find("<'go'> when 'true' ->")
        .expect("the dispatch clause of `go`");
    let rest = &code[start..];
    let end = rest.find("<OtherSelector>").unwrap_or(rest.len());
    &rest[..end]
}

#[test]
fn the_repro_threads_the_post_loop_local_and_state_out_of_the_block() {
    let code = codegen(REPRO);
    let sums = {
        let start = code.find("<'sums'> when 'true' ->").expect("sums clause");
        let rest = &code[start..];
        &rest[..rest.find("<OtherSelector>").unwrap_or(rest.len())]
    };
    // The loop is a producer: its construct tuple is bound, the state and the
    // local are read back out of it into the frame, and the block answers the
    // re-read local with that state.
    assert!(
        sums.contains("let _ThreadedResult"),
        "the loop should be lowered as a producer:\n{sums}"
    );
    assert!(
        sums.contains("= call 'maps':'get'('__local__n', StateAcc2) in"),
        "the post-loop local should be read from the post-loop state:\n{sums}"
    );
    assert!(
        sums.contains("{_T2Res") && sums.contains(", StateAcc2}"),
        "the block should return the post-loop state:\n{sums}"
    );
    assert!(
        !sums.contains("let _ = let _temp"),
        "the loop must not be an opaque discarded statement:\n{sums}"
    );
}

#[test]
fn the_unbroken_repro_passes_the_verifier() {
    assert_eq!(verifier_report(REPRO, None), None);
}

#[test]
fn withholding_the_producer_sites_is_producer_site_not_threaded() {
    let report = verifier_report(REPRO, Some(LocalRebindFault::Tier2OpaqueProducers))
        .expect("the opaque lowering is a verifier finding");
    assert!(
        report.contains("ProducerSiteNotThreaded"),
        "expected ProducerSiteNotThreaded, got: {report}"
    );
}

/// A field-writing loop alongside the local-writing one: both state families
/// are carried out of the block.
#[test]
fn a_loop_writing_a_field_and_a_local_carries_both() {
    let src = "Actor subclass: Q
  state: groups = #(#(1, 2), #(3))
  state: hits = 0
  sealed fwd: block => self.groups collect: block
  go => self fwd: [:xs | n := 0. xs do: [:y | n := n + y. self.hits := self.hits + y]. n]
";
    assert_eq!(verifier_report(src, None), None);
    let code = codegen(src);
    let go = go_clause(&code);
    assert!(go.contains("let _ThreadedResult"), "{go}");
    assert!(!go.contains("let _ = let _Packed"), "{go}");
}

/// Constructs that write the block-local `t`; the shared list the producer
/// agreement corpus uses (`local_producer_agreement`).
fn constructs() -> &'static [&'static str] {
    super::local_producer_agreement::CONSTRUCTS
}

/// Operand positions of a construct in a block body. Statement-position and
/// assignment-value positions, sequenced operands of a send, a literal and a
/// producer in the receiver of another.
const POSITIONS: &[&str] = &[
    "{c}. t",
    "r := {c}. #[r, t]",
    "r := ({c}) size. #[r, t]",
    "r := t + ({c}). #[r, t]",
    "r := #[{c}, t]. r",
    "r := 0 max: ({c}). #[r, t]",
    "r := ({c}) inject: 0 into: [:a :x | u := u + 1. a + x]. #[r, t, u]",
    "{c}. {c}. t",
    "{c}",
];

/// `go` wraps a promoted block whose body is `position` with `{c}` replaced
/// by `construct`, after binding the block-locals the constructs write.
fn program(construct: &str, position: &str) -> String {
    let body = position.replace("{c}", construct);
    format!(
        "Actor subclass: Corpus
  state: groups = #(#(1, 2), #(3))
  state: hits = 0
  sealed fwd: block => self.groups collect: block
  promote => self fwd: [:xs | self.hits := self.hits + 1. xs]
  go => self fwd: [:xs | t := 0. u := 0. s := 0. {body}]
"
    )
}

fn programs() -> Vec<String> {
    let mut out = Vec::new();
    for construct in constructs() {
        for position in POSITIONS {
            out.push(program(construct, position));
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

#[test]
fn every_corpus_program_compiles_clean() {
    let mut clean = 0;
    for src in programs() {
        match outcome(&src, None) {
            Outcome::Clean => clean += 1,
            Outcome::Unsupported => {}
            Outcome::Finding(report) => panic!("{report}\n{src}"),
        }
    }
    assert!(clean > 150, "only {clean} corpus programs compiled");
}

/// With the producer sites withheld, every corpus program in which a construct
/// threads a block-local is reported as `ProducerSiteNotThreaded` and nothing
/// else; a program the fault leaves clean has no producer site to lose.
#[test]
fn the_fault_is_reported_over_the_generated_corpus() {
    let mut reported = 0;
    for src in programs() {
        match outcome(&src, Some(LocalRebindFault::Tier2OpaqueProducers)) {
            Outcome::Finding(report) => {
                assert!(
                    report.contains("ProducerSiteNotThreaded"),
                    "unexpected finding {report}\n{src}"
                );
                reported += 1;
            }
            Outcome::Clean | Outcome::Unsupported => {}
        }
    }
    // Not vacuous: most cells hold a producer that writes `t`.
    assert!(
        reported > 100,
        "only {reported} corpus programs reported the fault"
    );
}
