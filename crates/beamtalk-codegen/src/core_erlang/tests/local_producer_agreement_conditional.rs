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
fn conditional_spans(body: &[ExpressionStatement]) -> HashMap<Span, Vec<Span>> {
    let mut out = HashMap::new();
    for stmt in body {
        beamtalk_core::ast_walker::walk_expression(&stmt.expression, &mut |e| {
            if let Some(arms) = conditional_arm_blocks(e) {
                out.insert(
                    Expression::span(e.unwrap_parens()),
                    arms.iter().map(|b| b.span).collect(),
                );
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
        // A loop or fold lowered as a producer inside an arm of a conditional
        // the core rule does not name would be lowered by an arm frame the
        // Phase 0 check never judged it under (it applies the legacy rows to
        // the arm): codegen registers an arm frame only for a conditional
        // producer.
        for (cond, arms) in &conditionals {
            let inside = arms
                .iter()
                .any(|a| a.start() <= span.start() && span.end() <= a.end());
            assert!(
                !inside || expected.contains_key(cond) || expected.contains_key(span),
                "codegen lowered the construct at {span:?} as a producer in an arm of \
                 the non-producer conditional at {cond:?}\n{src}"
            );
        }
        if conditionals.contains_key(span) {
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

/// Actor conditionals the core rule does not name as producers (the arm writes a
/// field or sends to `self` and writes no outer local, or holds a lookup whose
/// writes are not fully packed), with loops and folds in their arms. The
/// invariant [`check_conditional_agreement`] checks: codegen registers
/// `BranchArm` frames only for a conditional the core rule names, so a loop or
/// fold in the arm of any other conditional is judged and lowered by the legacy
/// rows, never by an arm frame.
const ACTOR_NON_PRODUCER_CONDITIONALS: &[&str] = &[
    "(s < 3) ifTrue: [self.n := 1. #(1) do: [:e | t := t + e]]",
    "(s < 3) ifTrue: [self.n := 1. 1 to: 2 do: [:i | t := t + i]]",
    "(s < 3) ifTrue: [self.n := 1. #(1) inject: 0 into: [:a :e | t := t + e. a + e]]",
    "(s < 3) ifTrue: [self.n := 1. [s < 2] whileTrue: [s := s + 1. t := t + 1]]",
    "(s < 3) ifTrue: [self go. #(1) do: [:e | t := t + e]]",
    "(s < 3) ifTrue: [self.n := 1. (s < 3) ifTrue: [self.n := 2. #(1) do: [:e | t := t + e]]]",
    "(s < 3) ifTrue: [#(1) do: [:e | self.n := e. t := t + e]]",
    "(s < 3) ifTrue: [self.n := 1] ifFalse: [#(1) do: [:e | t := t + e]]",
    "(s < 3) ifTrue: [self.n := t]",
    "(s < 3) ifTrue: [self.n := 1. t := t + 1. #(1) detect: [:e | e > 5] ifNone: [t := t + 1. 0]]",
    "(s < 3) ifTrue: [self.n := 1. #(1) collect: [:e | t := t + e]]",
    "s ifNil: [self.n := 1. #(1) do: [:e | t := t + e]]",
    "s ifNotNil: [:v | self go. 1 to: 2 do: [:i | t := t + i]]",
    "(s < 3) and: [self.n := 1. #(1) do: [:e | t := t + e]. true]",
    // Loops writing only a local of the arm: the conditional writes no outer
    // local (not a producer), yet its arm holds a producer-shaped loop.
    "(s < 3) ifTrue: [self.n := 1. m := 0. #(1, 2) do: [:e | m := m + e]. m]",
    "(s < 3) ifTrue: [self go. m := 0. 1 to: 2 do: [:i | m := m + i]. m] ifFalse: [0]",
    "(s < 3) ifTrue: [self.n := 1. m := 0. #(1, 2) inject: 0 into: [:a :e | m := m + e. a + e]]",
    "(s < 3) ifTrue: [self.n := 1. m := 0. [m < 2] whileTrue: [m := m + 1]. m]",
    "(s < 3) ifTrue: [self.n := 1. m := 0. (#(1, 2) collect: [:e | m := m + e]) size]",
];

/// The same, in every method context: conditionals with no outer-local write
/// whose arms hold a loop or fold writing only a local of the arm, nested in
/// another arm, and with a self-send. No field write (a value type cannot write
/// one, ADR 0042).
const NON_PRODUCER_CONDITIONALS: &[&str] = &[
    "(s < 3) ifTrue: [m := 0. #(1, 2) do: [:e | m := m + e]. m] ifFalse: [0]",
    "(s < 3) ifTrue: [m := 0. 1 to: 2 do: [:i | m := m + i]. m] ifFalse: [0]",
    "(s < 3) ifTrue: [m := 0. #(1, 2) inject: 0 into: [:a :e | m := m + e. a + e]. m]",
    "(s < 3) ifTrue: [m := 0. [m < 2] whileTrue: [m := m + 1]. m]",
    "(s < 3) ifTrue: [m := 0. (#(1, 2) collect: [:e | m := m + e]) size + m]",
    "(s < 3) ifTrue: [self go. m := 0. #(1, 2) do: [:e | m := m + e]. m]",
    "(s < 3) ifTrue: [(s < 3) ifTrue: [m := 0. #(1, 2) do: [:e | m := m + e]. m]]",
    "s ifNil: [m := 0. #(1, 2) do: [:e | m := m + e]. m] ifNotNil: [:v | v]",
    "(s < 3) and: [m := 0. #(1, 2) do: [:e | m := m + e]. true]",
];

#[test]
fn conditionals_that_are_not_producers_agree_with_the_core_rule_in_every_context() {
    let mut checked = 0;
    for construct in NON_PRODUCER_CONDITIONALS {
        for position in POSITIONS
            .iter()
            .chain(CONDITIONAL_POSITIONS)
            .filter(|p| !p.contains("super") && !p.contains(".blk"))
        {
            for src in sources(construct, position) {
                checked += check_conditional_agreement(&src);
            }
        }
    }
    assert!(checked > 0, "no accepted program compared");
}

#[test]
fn actor_conditionals_that_are_not_producers_agree_with_the_core_rule() {
    let mut checked = 0;
    for construct in ACTOR_NON_PRODUCER_CONDITIONALS {
        // (A `super` send's argument that carries a field write trips the
        // verifier's `StateEffectEscapesExpression` on main as well, a
        // `generate_super_send` gap independent of conditionals-as-producers.)
        for position in POSITIONS
            .iter()
            .chain(CONDITIONAL_POSITIONS)
            .filter(|p| !p.contains("super"))
        {
            let [_, _, actor] = sources(construct, position);
            checked += check_conditional_agreement(&actor);
        }
    }
    assert!(checked > 0, "no accepted program compared");
}
