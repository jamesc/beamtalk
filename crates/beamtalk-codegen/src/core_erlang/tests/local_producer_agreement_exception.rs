// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 3b (BT-3817): the producer half of section 6 for `on:do:` and
//! `ensure:`. The sibling of `local_producer_agreement_conditional.rs`, whose
//! checker it reuses: every exception construct the Phase 0 check accepts
//! because it is a local-threading producer (`beamtalk-core`'s
//! `exception_producer_writes`) at a method-root site, a method-body statement,
//! or a statement of an arm of another producer (a conditional's arm, a loop
//! or fold body, or the protected body, handler or cleanup of another
//! exception), is lowered by codegen's exception producer threading exactly the
//! locals the core rule names, and codegen lowers no exception construct as a
//! producer anywhere else (nor a loop or fold in a frame the core rule does not
//! open). A program the ADR 0131 checks reject is skipped.

use super::local_producer_agreement::{BODY_STATEMENTS, POSITIONS, body_sources, sources};
use super::local_producer_agreement_conditional::{
    ARM_CONSTRUCTS, CONDITIONAL_POSITIONS, check_conditional_agreement,
};

/// Exceptions that write `t`: every selector, protected-body, handler and
/// cleanup writes, two locals, loops, folds and conditionals in their blocks,
/// exceptions nested in one another, a `^`, and non-producers (an empty
/// handler, no local written) that the core rule must not name.
const EXCEPTIONS: &[&str] = &[
    "[t := t + 1. 2] on: Error do: [:e | 0]",
    "[2] on: Error do: [:e | t := t + 1. 0]",
    "[t := t + 1. 2] on: Error do: [:e | t := 9. 0]",
    "[t := t + 1. u := 5. 2] on: Error do: [:e | u := 6. 0]",
    "[t := t + 1. 2] ensure: [nil]",
    "[2] ensure: [t := t + 1]",
    "[t := t + 1] ensure: [u := u + 1]",
    "[#(1, 2) do: [:x | t := t + x]] on: Error do: [:e | 0]",
    "[t := t + 1] ensure: [#(1, 2) do: [:x | u := u + x]]",
    "[(s < 3) ifTrue: [t := t + 1]] ensure: [nil]",
    "[(s < 3) ifTrue: [t := t + 1. 2] ifFalse: [3]] on: Error do: [:e | 0]",
    "[[t := t + 1] ensure: [nil]] on: Error do: [:e | 0]",
    "[[t := t + 1] on: Error do: [:e | 0]] ensure: [u := u + 1]",
    "[t := t + 1. ^t] ensure: [nil]",
    "[2] on: Error do: [:e | 0]",
    "[2] ensure: [nil]",
    "[n := (#(1, 2) collect: [:x | t := t + x]) size. n] on: Error do: [:e | 0]",
    "[(s < 3) ifTrue: [t := t + 1]. 1] ensure: [(s < 3) ifTrue: [u := u + 1]]",
];

/// The frames an exception producer's blocks form: `{s}` holds a statement.
const EXCEPTION_FRAMES: &[&str] = &[
    "[{s}. 1] on: Error do: [:e | 0]",
    "[1] on: Error do: [:e | {s}. 0]",
    "[{s}. 1] ensure: [nil]",
    "[1] ensure: [{s}]",
    "[{s}] ensure: [{s}]",
    "[[{s}. 1] ensure: [nil]] on: Error do: [:e | 0]",
    "#(1, 2) do: [:k | [{s}. 1] ensure: [nil]]",
    "#(1, 2) do: [:k | [1] on: Error do: [:e | {s}. 0]]",
    "(s < 3) ifTrue: [[{s}. 1] ensure: [nil]]",
    "(s < 3) ifTrue: [t := t + 1. [{s}. 1] on: Error do: [:e | 0]]",
    "[(s < 3) ifTrue: [{s}]] ensure: [nil]",
    "[#(1, 2) do: [:k | {s}]] ensure: [nil]",
];

#[test]
fn every_exception_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for construct in EXCEPTIONS {
        for position in POSITIONS.iter().chain(CONDITIONAL_POSITIONS) {
            for (context, src) in sources(construct, position).iter().enumerate() {
                // `self.blk` is an actor's state: a class or value-type method
                // cannot read it (an unsupported feature unrelated to
                // exceptions), so those positions are actor-only.
                if position.contains(".blk") && context != 2 {
                    continue;
                }
                checked += check_conditional_agreement(src).unwrap_or(0);
            }
        }
    }
    assert!(
        checked > 300,
        "only {checked} exception producer sites compared"
    );
}

#[test]
fn every_exception_frame_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for frame in EXCEPTION_FRAMES {
        for construct in ARM_CONSTRUCTS {
            for statement in BODY_STATEMENTS {
                for src in body_sources(frame, construct, statement) {
                    checked += check_conditional_agreement(&src).unwrap_or(0);
                }
            }
        }
    }
    assert!(
        checked > 300,
        "only {checked} exception frame sites compared"
    );
}
