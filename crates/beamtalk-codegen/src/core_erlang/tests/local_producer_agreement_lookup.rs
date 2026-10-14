// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 2d (BT-3798): the producer half of §6 for the lookup
//! selectors. The sibling of `local_producer_agreement_conditional.rs`: a
//! `Dictionary` lookup (`at:ifAbsent:`, the one row of
//! `beamtalk-core`'s `LOOKUP_SHAPES` that is `dictionary_inline`) is a
//! conditional in the core rule's eyes — its handler is the arm
//! (`conditional_arm_blocks` / `lookup_arm_blocks`) — so the conditional
//! agreement check compares both directions for it: every lookup the Phase 0
//! check accepts because it is a local-threading producer (a method-root site,
//! a method-body statement, or a statement of an arm frame) is lowered by
//! codegen's producer threading exactly the locals the core rule names, and
//! codegen lowers no lookup as a producer anywhere else.
//!
//! The rest of the table must stay rejected, and the lookups the producer
//! cannot lower with it: the four selectors `Dictionary` does not implement,
//! a lookup in the body of a loop or fold, a handler that is not a
//! parameterless block literal, and the REPL.

use super::local_producer_agreement::{BODY_STATEMENTS, POSITIONS, body_sources, sources};
use super::local_producer_agreement_conditional::{
    ARM_CONSTRUCTS, ARM_FRAMES, CONDITIONAL_POSITIONS, check_conditional_agreement,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

/// Lookups that write `t`: read-write and write-only handlers, two locals, a
/// key that reads a local, a handler holding a loop, a fold, a conditional,
/// another lookup, an `on:do:` or a `^`, a parenthesised form, and the forms
/// the producer cannot lower (a parenthesised handler, a block key, a handler
/// with a parameter), which Phase 0 rejects, so they are skipped.
pub(super) const LOOKUPS: &[&str] = &[
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. 0]",
    "#{#a => 1} at: #a ifAbsent: [t := t + 1. 0]",
    "#{#a => 1} at: #k ifAbsent: [t := 7]",
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. u := u + 2. 0]",
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. u := 5. 0]",
    "#{#a => 1} at: s ifAbsent: [t := t + 1. 0]",
    "(#{#a => 1} at: #k ifAbsent: [t := t + 1. 0])",
    "#{#a => 1} at: #k ifAbsent: [#(1, 2) do: [:x | t := t + x]]",
    "#{#a => 1} at: #k ifAbsent: [n := (#(1, 2) collect: [:x | t := t + x]) size. 2]",
    "#{#a => 1} at: #k ifAbsent: [1 to: 2 do: [:i | t := t + i]. t]",
    "#{#a => 1} at: #k ifAbsent: [[s < 2] whileTrue: [s := s + 1. t := t + 1]]",
    "#{#a => 1} at: #k ifAbsent: [#(1, 2) inject: 0 into: [:a :x | t := t + x. a + x]]",
    "#{#a => 1} at: #k ifAbsent: [#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]]",
    "#{#a => 1} at: #k ifAbsent: [(s < 3) ifTrue: [t := t + 1. 1] ifFalse: [2]]",
    "#{#a => 1} at: #k ifAbsent: [s ifNil: [t := t + 1. 2]]",
    "#{#a => 1} at: #k ifAbsent: [#{#b => 2} at: #z ifAbsent: [t := t + 1. 1]]",
    "#{#a => 1} at: #k ifAbsent: [[t := t + 1] on: Error do: [:e | 0]]",
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. ^t]",
    "(#{#a => 1} at: #k ifAbsent: [t := t + 1. 0]) + 1",
    "#{#a => 1} at: (#{#b => 2} at: #z ifAbsent: [t := t + 1. #k]) ifAbsent: [u := u + 1. 0]",
    "#{#a => 1} at: #k ifAbsent: ([t := t + 1. 0])",
    "#{#a => 1} at: [t := t + 1. #k] ifAbsent: [0]",
    "#{#a => 1} at: #k ifAbsent: [:x | t := t + 1. 0]",
];

/// A cross-section of [`LOOKUPS`] for the arm-frame products, which multiply
/// by every frame and statement shape: read-write and write-only handlers,
/// two locals, a handler holding a loop, a fold, a lookup and an `on:do:`, a
/// `^`, a parenthesised form, and one the producer cannot lower.
const LOOKUP_SAMPLE: &[&str] = &[
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. 0]",
    "#{#a => 1} at: #k ifAbsent: [t := 7]",
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. u := u + 2. 0]",
    "(#{#a => 1} at: #k ifAbsent: [t := t + 1. 0])",
    "#{#a => 1} at: #k ifAbsent: [#(1, 2) do: [:x | t := t + x]]",
    "#{#a => 1} at: #k ifAbsent: [#{#b => 2} at: #z ifAbsent: [t := t + 1. 1]]",
    "#{#a => 1} at: #k ifAbsent: [[t := t + 1] on: Error do: [:e | 0]]",
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. ^t]",
    "#{#a => 1} at: #k ifAbsent: ([t := t + 1. 0])",
];

/// Lookups whose handler writes nothing: never producers.
const READ_ONLY_LOOKUPS: &[&str] = &[
    "#{#a => 1} at: #k ifAbsent: [0]",
    "#{#a => 1} at: #k ifAbsent: [t + 1]",
    "#{#a => 1} at: #k ifAbsent: [m := 0. m := m + 1. m]",
    "#{#a => 1} at: #k ifAbsent: [m := 0. #(1, 2) do: [:x | m := m + x]. m]",
];

/// The four lookup selectors `Dictionary` does not implement.
const UNIMPLEMENTED_LOOKUPS: &[&str] = &[
    "#{#a => 1} at: #k ifAbsentPut: [t := t + 1. 0]",
    "#{#a => 1} at: #k ifPresent: [:v | t := t + v]",
    "#{#a => 1} at: #k ifPresent: [:v | t := t + v. v] ifAbsent: [0]",
    "#{#a => 1} at: #k ifPresent: [:v | v] ifAbsent: [t := t + 1. 0]",
    "#{#a => 1} removeKey: #k ifAbsent: [t := t + 1. 0]",
];

/// The frames that are a lookup producer's arm (its handler), holding `{s}`.
const LOOKUP_ARM_FRAMES: &[&str] = &[
    "#{#a => 1} at: #k ifAbsent: [{s}]",
    "#{#a => 1} at: #k ifAbsent: [{s}. 0]",
    "#{#a => 1} at: #k ifAbsent: [t := t + 1. {s}. 0]",
    "#{#a => 1} at: #k ifAbsent: [#{#b => 2} at: #z ifAbsent: [{s}. 0]]",
    "(s < 3) ifTrue: [#{#a => 1} at: #k ifAbsent: [{s}. 0]]",
    "[#{#a => 1} at: #k ifAbsent: [{s}. 0]] on: Error do: [:e | 0]",
    "#{#a => 1} at: #k ifAbsent: [(s < 3) ifTrue: [{s}]]",
    "#{#a => 1} at: #k ifAbsent: [#(1, 2) do: [:k | {s}]]",
];

/// Frames that are a loop or fold body and not an arm: a lookup that writes
/// `t` is not a producer there.
const LOOP_ONLY_FRAMES: &[&str] = &[
    "1 to: 2 do: [:k | {s}]",
    "#(1, 2) do: [:k | {s}]",
    "2 timesRepeat: [{s}]",
    "[s < 3] whileTrue: [s := s + 1. {s}]",
    "#(1, 2) collect: [:k | {s}. k]",
    "#(1, 2) inject: 0 into: [:a :k | {s}. a + k]",
    "#(1, 2) do: [:k | 1 to: 2 do: [:i | {s}]]",
];

/// Whether the Phase 0 / §6 checks reject `src`.
fn rejected(src: &str) -> bool {
    let (module, parse_diags) = parse(lex_with_eof(src));
    assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
    analyse_full(&module, AnalysisContext::default())
        .diagnostics
        .iter()
        .any(|d| {
            d.severity == Severity::Error
                && matches!(
                    d.category,
                    Some(
                        DiagnosticCategory::UnmigratedLocalThreading
                            | DiagnosticCategory::Tier2BlockNoReturnChannel
                    )
                )
        })
}

#[test]
fn every_lookup_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for construct in LOOKUPS {
        // (The positions that carry a `super`/`.blk` send are in `POSITIONS`
        // for every construct: the sites stop there and Phase 0 rejects.)
        for position in POSITIONS.iter().chain(CONDITIONAL_POSITIONS) {
            for src in sources(construct, position) {
                checked += check_conditional_agreement(&src).unwrap_or(0);
            }
        }
    }
    assert!(
        checked > 300,
        "only {checked} lookup producer sites compared"
    );
}

#[test]
fn every_lookup_arm_frame_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for frame in LOOKUP_ARM_FRAMES {
        for construct in ARM_CONSTRUCTS.iter().chain(LOOKUP_SAMPLE) {
            for statement in BODY_STATEMENTS {
                for src in body_sources(frame, construct, statement) {
                    checked += check_conditional_agreement(&src).unwrap_or(0);
                }
            }
        }
    }
    assert!(
        checked > 300,
        "only {checked} lookup arm frame sites compared"
    );
}

#[test]
fn lookups_in_the_arms_of_other_conditional_producers_agree() {
    let mut checked = 0;
    for frame in ARM_FRAMES {
        for construct in LOOKUP_SAMPLE {
            for statement in BODY_STATEMENTS {
                for src in body_sources(frame, construct, statement) {
                    checked += check_conditional_agreement(&src).unwrap_or(0);
                }
            }
        }
    }
    assert!(checked > 300, "only {checked} arm frame lookups compared");
}

#[test]
fn lookups_that_write_nothing_agree_in_every_context() {
    let mut compiled = [0usize; 3];
    for construct in READ_ONLY_LOOKUPS {
        for position in POSITIONS
            .iter()
            .chain(CONDITIONAL_POSITIONS)
            .filter(|p| !p.contains("super") && !p.contains(".blk"))
        {
            for (i, src) in sources(construct, position).iter().enumerate() {
                compiled[i] += usize::from(check_conditional_agreement(src).is_some());
            }
        }
    }
    assert!(
        compiled.iter().all(|&n| n >= 10),
        "programs compiled and compared per context (class, value, actor): {compiled:?}"
    );
}

#[test]
fn a_lookup_in_a_loop_or_fold_body_is_never_a_producer() {
    // The lookup producer lowers at the method root and in arm frames only.
    // In a loop or fold body its handler's write would be dropped, so Phase 0
    // must reject every such program (codegen never gets one to lower).
    let mut rejected_count = 0;
    for frame in LOOP_ONLY_FRAMES {
        for construct in LOOKUPS.iter().filter(|c| !c.contains("[:x |")) {
            for statement in [
                "{c}",
                "({c})",
                "r := {c}",
                "r := ({c}) size",
                "r := t + ({c})",
            ] {
                for src in body_sources(frame, construct, statement) {
                    // A lookup whose handler is the only writer in the body.
                    if rejected(&src) {
                        rejected_count += 1;
                    } else {
                        // Accepted: the only way is that the lookup writes no
                        // outer local (a handler of `t` locals only).
                        assert!(
                            !construct.contains("t := ") && !construct.contains("u := "),
                            "Phase 0 accepted a handler-writing lookup in a loop or fold body \
                             (its write would be dropped):\n{src}"
                        );
                    }
                }
            }
        }
    }
    assert!(
        rejected_count > 100,
        "only {rejected_count} rejections checked"
    );
}

#[test]
fn lookup_selectors_that_dictionary_does_not_implement_stay_rejected() {
    // `Dictionary` implements `at:ifAbsent:` only, so there is no behaviour for
    // an inline lowering of the other four to answer: a handler that writes an
    // outer local keeps its §6 error in every position and method context.
    let mut checked = 0;
    for construct in UNIMPLEMENTED_LOOKUPS {
        for position in POSITIONS.iter().chain(CONDITIONAL_POSITIONS) {
            for src in sources(construct, position) {
                assert!(rejected(&src), "{src}");
                checked += 1;
            }
        }
    }
    assert!(checked > 100, "only {checked} programs checked");
}
