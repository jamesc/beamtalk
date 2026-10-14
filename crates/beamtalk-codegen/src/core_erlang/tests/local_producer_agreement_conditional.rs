// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 3 (BT-3738): the producer half of §6 for conditionals. The
//! sibling of `local_producer_agreement.rs` (loops and folds): every
//! conditional (`ifTrue:` … `ifNil:ifNotNil:`, `and:`/`or:`) the Phase 0
//! check accepts because it is a local-threading producer
//! (`beamtalk-core`'s `conditional_producer_writes`) at a method-root site, a
//! method-body statement, or a statement of an arm of another such producer
//! (a `BranchArm` frame, `LoopBodyContext::arm`), is lowered by codegen's
//! conditional producer threading exactly the locals the core rule names, and
//! codegen lowers no conditional as a producer anywhere else.
//!
//! The corpus pairs each conditional that writes `t` with every operand and
//! statement position, in each method context, and each loop or fold that
//! writes `t` with every statement of an arm frame; a program the ADR 0131
//! checks reject is skipped (nothing is compiled for it).

use super::local_producer_agreement::{
    BODY_STATEMENTS, POSITIONS, body_sources, core_body_producers, core_producers, sources,
};
use crate::core_erlang::threaded_expr::fault::recording_producers;
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::{Expression, ExpressionStatement};
use beamtalk_core::semantic_analysis::block_facts::{
    LoopBodyContext, RootMethodKind, conditional_arm_blocks, conditional_producer_writes,
    is_loop_body_producer_site, loop_body_producer_sites, operand_producer_writes,
    sequenced_operand_sites,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::{HashMap, HashSet};

/// Conditionals that write `t`: every selector, read-write and write-only
/// arms, two locals, and arms holding a loop, a fold, a nested conditional,
/// a nested non-producer (rejected by Phase 0, so skipped) or a `^`.
pub(super) const CONDITIONALS: &[&str] = &[
    "(s < 3) ifTrue: [t := t + 1. 2] ifFalse: [3]",
    "(s < 3) ifTrue: [t := t + 1]",
    "(s < 3) ifFalse: [t := t + 1. 2]",
    "(s < 3) and: [t := t + 1. true]",
    "(s < 3) or: [t := t + 1. true]",
    "s ifNil: [t := t + 1. 2]",
    "s ifNotNil: [:v | t := t + v. 2]",
    "s ifNil: [t := t + 1. 0] ifNotNil: [:v | t := t + 2. v]",
    "s ifNotNil: [:v | t := t + 2. v] ifNil: [t := t + 1. 0]",
    "(s < 3) ifTrue: [t := 9. 2] ifFalse: [3]",
    "(s < 3) ifTrue: [t := t + 1. u := 5. 2] ifFalse: [u := 6. 3]",
    "(s < 3) ifTrue: [t := t + 1] ifFalse: [u := u + 1]",
    "(s < 3) ifTrue: [#(1, 2) do: [:x | t := t + x]]",
    "(s < 3) ifTrue: [n := (#(1, 2) collect: [:x | t := t + x]) size. 2] ifFalse: [3]",
    "(s < 3) ifTrue: [1 to: 2 do: [:i | t := t + i]. t := t + 100]",
    "(s < 3) ifTrue: [[s < 2] whileTrue: [s := s + 1. t := t + 1]]",
    "(s < 3) ifTrue: [(s < 3) ifTrue: [t := t + 1. 1] ifFalse: [2]] ifFalse: [3]",
    "(s < 3) ifTrue: [(s < 3) ifTrue: [t := t + 1]]",
    "(s < 3) ifTrue: [t := t + 1. (s < 3) ifTrue: [u := u + 1]]",
    "(s < 3) ifTrue: [#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1]]",
    "(s < 3) ifTrue: [#(1, 2) do: [:x | (s < 3) ifTrue: [t := t + x]]]",
    "(s < 3) ifTrue: [t := t + 1. ^t]",
    "(s < 3) ifTrue: [[t := t + 1] on: Error do: [:e | 0]]",
    "(s < 3) ifTrue: [n := (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]) + 1. 2] ifFalse: [3]",
    "(s < 3) ifTrue: [(#(1, 2) collect: [:x | t := t + x]) size]",
];

/// [`POSITIONS`] plus the positions a conditional is a producer in without
/// being an operand: a method-body statement (bare, parenthesised, last, and
/// followed by a read) and an assignment's whole value.
const CONDITIONAL_POSITIONS: &[&str] = &[
    "{c}\nt",
    "({c})\nt",
    "{c}",
    "r := ({c})\n#[r, t]",
    "t := t + 1\n{c}\nt",
    "{c}\n{c}\nt",
    "r := {c}\n{c}\n#[r, t]",
];

/// The arm frames: a conditional producer's arm holding `{s}`.
const ARM_FRAMES: &[&str] = &[
    "(s < 3) ifTrue: [{s}]",
    "(s < 3) ifTrue: [{s}] ifFalse: [nil]",
    "(s < 3) ifFalse: [{s}]",
    "(s < 3) and: [{s}. true]",
    "(s < 3) or: [{s}. true]",
    "s ifNil: [{s}] ifNotNil: [:v | v]",
    "s ifNotNil: [:v | {s}]",
    "(s < 3) ifTrue: [(s < 3) ifTrue: [{s}]]",
    "(s < 3) ifTrue: [t := t + 1. {s}]",
    "(s < 3) ifTrue: [#(1, 2) do: [:k | {s}]]",
    "(s < 3) ifTrue: [r := (s < 3) ifTrue: [{s}. 1] ifFalse: [2]. r]",
    "#(1, 2) do: [:k | (s < 3) ifTrue: [{s}]]",
    "2 timesRepeat: [(s < 3) ifTrue: [#(1, 2) do: [:i | {s}]]]",
];

/// Constructs used inside arm frames (a subset of [`CONSTRUCTS`] that keeps the
/// corpus small).
const ARM_CONSTRUCTS: &[&str] = &[
    "#(1, 2) collect: [:x | t := t + x]",
    "#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
    "1 to: 2 do: [:i | t := t + i]",
    "[s < 2] whileTrue: [s := s + 1. t := t + 1]",
    "#(1, 2) do: [:x | t := t + x]",
    "#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]",
    "(s < 3) ifTrue: [t := t + 1. 2] ifFalse: [3]",
    "s ifNil: [t := t + 1. 2]",
];

fn kind_of(src: &str) -> RootMethodKind {
    if src.starts_with("Actor") {
        RootMethodKind::Actor
    } else if src.starts_with("Value") {
        RootMethodKind::ValueType
    } else {
        RootMethodKind::Class
    }
}

fn names_of(
    writes: Vec<beamtalk_core::semantic_analysis::block_facts::OuterLocalWrite>,
) -> Vec<String> {
    let mut names: Vec<String> = writes.into_iter().map(|w| w.name.to_string()).collect();
    names.sort();
    names.dedup();
    names
}

/// The producers of the conditional `cond`'s arms, recursively: for each arm
/// statement the sites the loop-body rule names in an arm frame
/// (`LoopBodyContext::in_arm`), a loop or fold or a nested conditional that
/// is a producer there.
fn arm_producers(
    cond: &Expression,
    kind: RootMethodKind,
    in_actor: bool,
    out: &mut HashMap<Span, Vec<String>>,
    value_bearing: &mut HashSet<Span>,
) {
    let bound = |name: &str| matches!(name, "t" | "u" | "s" | "r");
    let Some(arms) = conditional_arm_blocks(cond) else {
        return;
    };
    for arm in arms {
        for inner in &arm.body {
            let ctx = LoopBodyContext::new(&inner.expression, false, false, in_actor).in_arm();
            for site in loop_body_producer_sites(&inner.expression) {
                if !is_loop_body_producer_site(site, &ctx) {
                    continue;
                }
                if let Some(writes) = operand_producer_writes(site, &bound) {
                    out.insert(Expression::span(site), names_of(writes));
                    core_body_producers(site, false, in_actor, out, value_bearing);
                } else if let Some(writes) = conditional_producer_writes(site, &bound, kind) {
                    out.insert(Expression::span(site), names_of(writes));
                    arm_producers(site, kind, in_actor, out, value_bearing);
                }
            }
        }
    }
}

/// The core rule's verdict for every conditional producer of a method body:
/// at each sequenced operand site, as a statement of the body, and (through
/// [`arm_producers`]) in the arms of each; plus the loop and fold producers
/// of [`core_producers`] and of the loop bodies reachable from the statements.
fn core_conditional_producers(
    body: &[ExpressionStatement],
    kind: RootMethodKind,
    in_actor: bool,
) -> HashMap<Span, Vec<String>> {
    let bound = |name: &str| matches!(name, "t" | "u" | "s" | "r");
    let mut out = core_producers(body);
    let mut value_bearing = HashSet::new();
    for stmt in body {
        let mut candidates = sequenced_operand_sites(&stmt.expression);
        candidates.push(stmt.expression.unwrap_parens());
        for site in candidates {
            if let Some(writes) = conditional_producer_writes(site, &bound, kind) {
                out.insert(Expression::span(site), names_of(writes));
                arm_producers(site, kind, in_actor, &mut out, &mut value_bearing);
            }
        }
        core_body_producers(
            &stmt.expression,
            false,
            in_actor,
            &mut out,
            &mut value_bearing,
        );
    }
    out
}

/// Every conditional (an expression `conditional_arm_blocks` names) in the
/// method body, by span.
fn conditional_spans(body: &[ExpressionStatement]) -> HashSet<Span> {
    let mut out = HashSet::new();
    for stmt in body {
        beamtalk_core::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if conditional_arm_blocks(e).is_some() {
                out.insert(Expression::span(e.unwrap_parens()));
            }
        });
    }
    out
}

/// Compares the core rule and codegen for one program, both ways; a program
/// the Phase 0 checks reject is skipped. Returns the number of producer sites
/// compared.
fn check_conditional_agreement(src: &str) -> usize {
    let (module, parse_diags) = parse(lex_with_eof(src));
    assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
    let analysis = analyse_full(&module, AnalysisContext::default());
    let rejected = analysis.diagnostics.iter().any(|d| {
        d.severity == Severity::Error
            && matches!(
                d.category,
                Some(
                    DiagnosticCategory::UnmigratedLocalThreading
                        | DiagnosticCategory::Tier2BlockNoReturnChannel
                )
            )
    });
    if rejected {
        return 0;
    }
    let class = &module.classes[0];
    let method = class
        .methods
        .iter()
        .chain(&class.class_methods)
        .next()
        .expect("one method");
    let kind = kind_of(src);
    let expected = core_conditional_producers(&method.body, kind, kind == RootMethodKind::Actor);
    let (generated, records) = recording_producers(|| {
        generate_module_with_warnings(&module, CodegenOptions::new("agree"))
    });
    assert!(generated.is_ok(), "{src}: {:?}", generated.err());
    let lowered: HashMap<Span, Vec<String>> = records
        .into_iter()
        .filter(|(_, names)| !names.is_empty())
        .collect();
    let conditionals = conditional_spans(&method.body);
    let mut checked = 0;
    for (span, names) in &expected {
        assert_eq!(
            lowered.get(span),
            Some(names),
            "the core rule accepts the construct at {span:?} as a producer threading \
             {names:?}, but codegen lowered {lowered:?}\n{src}"
        );
        checked += 1;
    }
    // And the other way: codegen lowers no conditional as a producer that the
    // core rule does not name.
    for (span, names) in &lowered {
        if conditionals.contains(span) {
            assert!(
                expected.contains_key(span),
                "codegen lowered the conditional at {span:?} as a producer threading \
                 {names:?}, but the core rule names {expected:?}\n{src}"
            );
        }
    }
    checked
}

#[test]
fn every_conditional_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for construct in CONDITIONALS {
        for position in POSITIONS.iter().chain(CONDITIONAL_POSITIONS) {
            for src in sources(construct, position) {
                checked += check_conditional_agreement(&src);
            }
        }
    }
    assert!(
        checked > 300,
        "only {checked} conditional producer sites compared"
    );
}

#[test]
fn every_arm_frame_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for frame in ARM_FRAMES {
        for construct in ARM_CONSTRUCTS {
            for statement in BODY_STATEMENTS {
                for src in body_sources(frame, construct, statement) {
                    checked += check_conditional_agreement(&src);
                }
            }
        }
    }
    assert!(checked > 300, "only {checked} arm frame sites compared");
}
