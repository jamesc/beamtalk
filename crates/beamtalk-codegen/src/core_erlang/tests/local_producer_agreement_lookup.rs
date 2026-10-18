// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 2d (BT-3798, BT-3824): the producer half of §6 for the
//! lookup selectors. The sibling of `local_producer_agreement_conditional.rs`:
//! a `Dictionary` lookup (`at:ifAbsent:`, `at:ifPresent:`,
//! `at:ifPresent:ifAbsent:`, the rows of `beamtalk-core`'s `LOOKUP_SHAPES`
//! that are `dictionary_inline`) is a
//! conditional in the core rule's eyes — its handler is the arm
//! (`conditional_arm_blocks` / `lookup_arm_blocks`) — so the conditional
//! agreement check compares both directions for it: every lookup the Phase 0
//! check accepts because it is a local-threading producer (a method-root site,
//! a method-body statement, a statement of an arm frame, or, since BT-3839, a
//! producer site of a loop or fold body) is lowered by codegen's producer
//! threading exactly the locals the core rule names, and codegen lowers no
//! lookup as a producer anywhere else.
//!
//! The rest of the table must stay rejected, and the lookups the producer
//! cannot lower with it: the two selectors `Dictionary` does not implement,
//! a lookup inside a closure that is not inlined, a handler that is not a
//! block literal taking the parameters the primitive passes, and the REPL.

use super::local_producer_agreement::{BODY_STATEMENTS, POSITIONS, body_sources, sources};
use super::local_producer_agreement_conditional::{
    ARM_CONSTRUCTS, ARM_FRAMES, CONDITIONAL_POSITIONS, LOOP_BODY_FRAMES,
    check_conditional_agreement,
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
    // BT-3824: the present handler takes the found value.
    "#{#a => 1} at: #k ifPresent: [:v | t := t + v]",
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. v]",
    "#{#a => 1} at: #a ifPresent: [:v | t := v]",
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. u := u + v. v]",
    "#{#a => 1} at: s ifPresent: [:v | t := t + v. 0]",
    "(#{#a => 1} at: #a ifPresent: [:v | t := t + v. v])",
    "#{#a => 1} at: #a ifPresent: [:v | #(1, 2) do: [:x | t := t + x + v]]",
    "#{#a => 1} at: #a ifPresent: [:v | (s < 3) ifTrue: [t := t + v. 1] ifFalse: [2]]",
    "#{#a => 1} at: #a ifPresent: [:v | #{#b => 2} at: #b ifPresent: [:w | t := t + v + w]]",
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. ^t]",
    "(#{#a => 1} at: #a ifPresent: [:v | t := t + v. v]) + 1",
    "#{#a => 1} at: #k ifPresent: [:v | t := t + v. v] ifAbsent: [0]",
    "#{#a => 1} at: #k ifPresent: [:v | v] ifAbsent: [t := t + 1. 0]",
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. v] ifAbsent: [u := u + 1. 0]",
    "#{#a => 1} at: #a ifPresent: [:v | t := v. v] ifAbsent: [t := 9. 0]",
    "(#{#a => 1} at: #a ifPresent: [:v | v] ifAbsent: [t := t + 1. 0]) + t",
    "#{#a => 1} at: #k ifPresent: [t := t + 1. 0]",
    "#{#a => 1} at: #k ifPresent: [:v | v] ifAbsent: [:x | t := t + 1. 0]",
    "#{#a => 1} at: #k ifPresent: ([:v | t := t + v. 0])",
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
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. v]",
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. v] ifAbsent: [u := u + 1. 0]",
    "#{#a => 1} at: #k ifPresent: [:v | v] ifAbsent: [t := t + 1. 0]",
];

/// Lookups whose handler writes nothing: never producers.
const READ_ONLY_LOOKUPS: &[&str] = &[
    "#{#a => 1} at: #k ifAbsent: [0]",
    "#{#a => 1} at: #k ifAbsent: [t + 1]",
    "#{#a => 1} at: #k ifAbsent: [m := 0. m := m + 1. m]",
    "#{#a => 1} at: #k ifAbsent: [m := 0. #(1, 2) do: [:x | m := m + x]. m]",
    "#{#a => 1} at: #a ifPresent: [:v | v + t]",
    "#{#a => 1} at: #a ifPresent: [:v | m := v. m := m + 1. m] ifAbsent: [t + 1]",
];

/// The two lookup selectors `Dictionary` does not implement (permanently: an
/// immutable `Dictionary` has no `at:ifAbsentPut:` or `removeKey:ifAbsent:`).
const UNIMPLEMENTED_LOOKUPS: &[&str] = &[
    "#{#a => 1} at: #k ifAbsentPut: [t := t + 1. 0]",
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
    "#{#a => 1} at: #a ifPresent: [:v | {s}. v]",
    "#{#a => 1} at: #a ifPresent: [:v | {s}. v] ifAbsent: [0]",
    "#{#a => 1} at: #k ifPresent: [:v | v] ifAbsent: [{s}. 0]",
    "#{#a => 1} at: #a ifPresent: [:v | t := t + v. 0] ifAbsent: [{s}. 0]",
];

/// BT-3839: frames where a handler-writing lookup sits inside a closure that is
/// not inlined (a block evaluated with an argument, a stored block), alone and
/// in a loop body. The lookup producer has no frame there, so Phase 0 keeps
/// its §6 error (ADR 0131 §6).
const CLOSURE_FRAMES: &[&str] = &[
    "[:z | {s}] value: 1",
    "b := [{s}]\nb value",
    "#(1, 2) do: [:k | [:z | {s}] value: k]",
    "1 to: 2 do: [:k | b := [{s}]. b value]",
];

/// Whether the Phase 0 / §6 checks reject `src`.
pub(super) fn rejected(src: &str) -> bool {
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

/// BT-3839: both ways, for a lookup anywhere a loop or fold body statement can
/// hold one (the statement, an assignment's value, an operand), in every loop
/// and fold frame (`LOOP_BODY_FRAMES`: counted, `whileTrue:`, each fold
/// accumulator shape, a loop in a fold and a fold in a loop): the core rule's
/// `is_loop_body_producer_site` + `conditional_producer_writes` name exactly
/// the lookups codegen lowers as local-threading producers in the frame, and
/// the enclosing loops and folds that thread their writes, in class,
/// value-type and actor methods.
#[test]
fn every_loop_body_lookup_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = [0usize; 3];
    for frame in LOOP_BODY_FRAMES {
        for construct in LOOKUP_SAMPLE {
            for statement in BODY_STATEMENTS {
                for (i, src) in body_sources(frame, construct, statement).iter().enumerate() {
                    checked[i] += check_conditional_agreement(src).unwrap_or(0);
                }
            }
        }
    }
    assert!(
        checked.iter().all(|&n| n > 150),
        "loop-body lookup producer sites compared (class, value, actor): {checked:?}"
    );
}

/// BT-3839: a lookup in a loop or fold body inside an arm of a conditional
/// producer, which is where the actor fold-in-arm agreement gap showed: the
/// enclosing loop must thread the handler's write, not only the lookup.
#[test]
fn loop_body_lookups_in_arm_frames_agree() {
    let frames = [
        "(s < 3) ifTrue: [#(1, 2) do: [:k | {s}]]",
        "(s < 3) ifTrue: [1 to: 2 do: [:k | {s}]] ifFalse: [nil]",
        "(s < 3) ifTrue: [#(1, 2) inject: 0 into: [:a :k | {s}. a + k]]",
        "[#(1, 2) do: [:k | {s}]] on: Error do: [:e | 0]",
        "#{#a => 1} at: #k ifAbsent: [#(1, 2) do: [:k | {s}]. 0]",
        "#(1, 2) do: [:k | (s < 3) ifTrue: [#(1, 2) do: [:i | {s}]]]",
    ];
    let mut checked = [0usize; 3];
    for frame in frames {
        for construct in LOOKUP_SAMPLE {
            for statement in BODY_STATEMENTS {
                for (i, src) in body_sources(frame, construct, statement).iter().enumerate() {
                    checked[i] += check_conditional_agreement(src).unwrap_or(0);
                }
            }
        }
    }
    assert!(
        checked.iter().all(|&n| n > 100),
        "arm-frame loop-body lookup sites compared (class, value, actor): {checked:?}"
    );
}

/// BT-3839: a handler-writing lookup in a loop or fold body that the producer
/// cannot lower (a parenthesised handler, a block key, a handler with the
/// wrong parameter count), and any handler-writing lookup inside a closure
/// that is not inlined ([`CLOSURE_FRAMES`]), keeps its Phase 0 / §6 error in
/// every method context: codegen never gets one to lower, so its write is
/// never silently dropped.
#[test]
fn lookups_the_producer_cannot_lower_stay_rejected_in_loop_bodies_and_closures() {
    let cannot_lower = [
        "#{#a => 1} at: #k ifAbsent: ([t := t + 1. 0])",
        "#{#a => 1} at: [t := t + 1. #k] ifAbsent: [0]",
        "#{#a => 1} at: #k ifAbsent: [:x | t := t + 1. 0]",
        "#{#a => 1} at: #k ifPresent: [t := t + 1. 0]",
        "#{#a => 1} at: #k ifPresent: ([:v | t := t + v. 0])",
    ];
    let statements = [
        "{c}",
        "({c})",
        "r := {c}",
        "r := ({c}) size",
        "r := t + ({c})",
    ];
    let mut checked = 0;
    for frame in LOOP_BODY_FRAMES {
        for construct in cannot_lower {
            for statement in statements {
                for src in body_sources(frame, construct, statement) {
                    assert!(rejected(&src), "{src}");
                    checked += 1;
                }
            }
        }
    }
    for frame in CLOSURE_FRAMES {
        for construct in LOOKUP_SAMPLE.iter().chain(&cannot_lower) {
            for statement in statements {
                for src in body_sources(frame, construct, statement) {
                    assert!(rejected(&src), "{src}");
                    checked += 1;
                }
            }
        }
    }
    assert!(checked > 1000, "only {checked} programs checked");
}

#[test]
fn lookup_selectors_that_dictionary_does_not_implement_stay_rejected() {
    // `Dictionary` does not implement `at:ifAbsentPut:` or
    // `removeKey:ifAbsent:`, so there is no behaviour for an inline lowering of
    // them to answer: a handler that writes an outer local keeps its §6 error
    // in every position and method context.
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

/// Receivers statically known not to be a `Dictionary` (BT-3798 review): the
/// runtime guard would raise on every call, so Phase 0 keeps the §6 error and
/// codegen is never asked to lower them as producers.
const NON_DICTIONARY_RECEIVER_LOOKUPS: &[&str] = &[
    "self at: #k ifAbsent: [t := t + 1. 0]",
    "super at: #k ifAbsent: [t := t + 1. 0]",
    "(self at: #k ifAbsent: [t := t + 1. 0])",
    "self at: #k ifAbsent: [t := 7]",
    "self at: #k ifPresent: [:v | t := t + v. 0]",
    "super at: #k ifPresent: [:v | v] ifAbsent: [t := t + 1. 0]",
];

#[test]
fn lookups_on_a_statically_non_dictionary_receiver_are_rejected_in_every_position() {
    let mut checked = 0;
    for construct in NON_DICTIONARY_RECEIVER_LOOKUPS {
        for position in POSITIONS.iter().chain(CONDITIONAL_POSITIONS) {
            for src in sources(construct, position) {
                assert!(rejected(&src), "{src}");
                assert_eq!(check_conditional_agreement(&src), None, "{src}");
                checked += 1;
            }
        }
    }
    assert!(checked > 100, "only {checked} programs checked");
}

/// BT-3824: the `at:ifPresent:` and `at:ifPresent:ifAbsent:` handlers that
/// write an outer local are producers (accepted by Phase 0 and lowered by the
/// lookup producer) in every method context, unless the producer cannot lower
/// them (a handler with the wrong parameter count, a parenthesised handler).
#[test]
fn if_present_lookups_are_producers_in_every_context() {
    let cannot_lower = [
        "#{#a => 1} at: #k ifPresent: [t := t + 1. 0]",
        "#{#a => 1} at: #k ifPresent: [:v | v] ifAbsent: [:x | t := t + 1. 0]",
        "#{#a => 1} at: #k ifPresent: ([:v | t := t + v. 0])",
    ];
    let mut checked = 0;
    for construct in LOOKUPS.iter().filter(|c| c.contains("ifPresent:")) {
        for src in sources(construct, "r := {c}\n#[r, t]") {
            if cannot_lower.contains(construct) {
                assert!(rejected(&src), "{src}");
            } else {
                let sites = check_conditional_agreement(&src);
                assert!(sites.is_some_and(|n| n > 0), "{src}: {sites:?}");
                checked += 1;
            }
        }
    }
    assert!(checked >= 45, "only {checked} programs checked");
}
