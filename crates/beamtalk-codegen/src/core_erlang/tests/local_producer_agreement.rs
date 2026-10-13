// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 §6 "One predicate, in `beamtalk-core`" — the producer half
//! (BT-3749, from the BT-3746 review): every loop or fold the Phase 0 check
//! accepts at a sequenced operand site because it is a local-threading
//! producer (`beamtalk-core`'s `sequenced_operand_sites` +
//! `operand_producer_writes`) is lowered by codegen's producer, threading
//! exactly the locals the core rule names. Otherwise the diagnostic would
//! accept a program whose construct tuple is never unpacked.
//!
//! The corpus is every pairing of a construct that writes `t` with an
//! operand position, in each method context; a program the ADR 0131
//! checks reject is skipped (nothing is compiled for it).

use crate::core_erlang::threaded_expr::fault::recording_producers;
use crate::core_erlang::{CodegenOptions, generate_module_with_warnings};
use beamtalk_core::ast::Expression;
use beamtalk_core::semantic_analysis::block_facts::{
    is_threaded_frame_body, loop_body_producer_sites, operand_producer_writes,
    sequenced_operand_sites,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::{HashMap, HashSet};

const CONSTRUCTS: &[&str] = &[
    "#(1, 2) collect: [:x | t := t + x]",
    "#(1, 2) reject: [:x | t := t + 1. false]",
    "#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x]",
    "#(1, 2) count: [:x | t := t + 1. true]",
    "1 to: 2 do: [:i | t := t + i]",
    "2 timesRepeat: [t := t + 1]",
    "[s < 2] whileTrue: [s := s + 1. t := t + 1]",
    "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [0]",
    "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [t := t + 1. 0]",
    "#(1, 2) detect: [:x | t := t + 1. x > 5] ifNone: [u := 1. 0]",
    "#(1, 2) do: [:x | t := t + x]",
    "#(1, 2) anySatisfy: [:x | t := t + 1. false]",
    "#(1, 2) select: [:x | t := t + 1. true]",
    "#(1, 2) detect: [:x | t := t + 1. x > 1]",
    "[s < 2] whileFalse: [s := s + 1. t := t + 1]",
    "1 to: 5 by: 2 do: [:i | t := t + i]",
];

const POSITIONS: &[&str] = &[
    "r := ({c}) size\n#[r, t]",
    "r := t + ({c})\n#[r, t]",
    "r := #[{c}, t]\nr",
    "r := {c}\n#[r, t]",
    "r := 0 max: ({c})\n#[r, t]",
    "Transcript show: ({c}) printString\nt",
    "^({c}) printString",
    // A producer in the receiver of another producer, in `^` (the
    // `Discard` tail) and in an assignment (the `Rebind` tail) position.
    "^({c}) inject: 0 into: [:a :x | u := u + 1. a + x]",
    "r := ({c}) inject: 0 into: [:a :x | u := u + 1. a + x]\n#[r, t, u]",
    // Sends whose statement lowering does not sequence its operands: the
    // sites stop there, so these stay Phase 0 errors (skipped below) or
    // compile with no producer.
    "r := super respondsTo: ({c}) printString\nr",
    "^super respondsTo: ({c}) printString",
    "self error: ({c}) printString\nt",
    "r := self perform: #size with: ({c})\nr",
    "r := self.blk valueWithArguments: ({c})\nr",
    "^self.blk valueWithArguments: ({c})",
    "self.blk valueWithArguments: ({c})\nt",
];

fn sources(construct: &str, position: &str) -> [String; 3] {
    let mut body = String::new();
    for line in format!(
        "t := 0\nu := 0\ns := 0\n{}",
        position.replace("{c}", construct)
    )
    .lines()
    {
        body.push_str("    ");
        body.push_str(line);
        body.push('\n');
    }
    [
        format!("Object subclass: AgreeClass\n  class go =>\n{body}"),
        format!("Value subclass: AgreeValue\n  go =>\n{body}"),
        format!(
            "Actor subclass: AgreeActor\n  state: n = 0\n  state: blk = nil\n\n  go =>\n{body}"
        ),
    ]
}

/// The core rule's verdict for every sequenced operand site of the method
/// body that writes an outer local: its span, and the names it threads when
/// it is a producer.
fn core_producers(body: &[beamtalk_core::ast::ExpressionStatement]) -> HashMap<Span, Vec<String>> {
    let bound = |name: &str| matches!(name, "t" | "u" | "s" | "r");
    let mut out = HashMap::new();
    for stmt in body {
        for site in sequenced_operand_sites(&stmt.expression) {
            if let Some(writes) = operand_producer_writes(site, &bound) {
                let mut names: Vec<String> =
                    writes.into_iter().map(|w| w.name.to_string()).collect();
                names.sort();
                names.dedup();
                out.insert(Expression::span(site), names);
            }
        }
    }
    out
}

/// The constructs a statement lowers itself rather than as a producer *site*:
/// the statement, the value of `x := V` and of `^V`, for the method body's
/// statements and those of every loop or fold body nested in them. Codegen
/// lowers a loop or fold there through the same construct tuple (a method-body
/// assignment's whole value), which the sites rule does not name.
fn whole_values(stmt: &Expression, out: &mut HashSet<Span>) {
    let inner = stmt.unwrap_parens();
    out.insert(Expression::span(inner));
    match inner {
        Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
            out.insert(Expression::span(value.unwrap_parens()));
        }
        _ => {}
    }
    let value = match inner {
        Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
            value.unwrap_parens()
        }
        other => other,
    };
    let Expression::MessageSend { arguments, .. } = value else {
        return;
    };
    for arg in arguments {
        if let Expression::Block(block) = arg {
            for inner in &block.body {
                whole_values(&inner.expression, out);
            }
        }
    }
}

#[test]
fn every_operand_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for construct in CONSTRUCTS {
        for position in POSITIONS {
            for src in sources(construct, position) {
                let (module, parse_diags) = parse(lex_with_eof(&src));
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
                    continue;
                }
                let class = &module.classes[0];
                // A program the checks accept must compile.
                let method = class.methods.iter().chain(&class.class_methods).next();
                let expected = core_producers(&method.expect("one method").body);
                let (generated, records) = recording_producers(|| {
                    generate_module_with_warnings(&module, CodegenOptions::new("agree"))
                });
                assert!(generated.is_ok(), "{src}: {:?}", generated.err());
                let lowered: HashMap<Span, Vec<String>> = records
                    .into_iter()
                    .filter(|(_, names)| !names.is_empty())
                    .collect();
                for (span, names) in &expected {
                    assert_eq!(
                        lowered.get(span),
                        Some(names),
                        "the core rule accepts the construct at {span:?} as a producer \
                         threading {names:?}, but codegen lowered {lowered:?}\n{src}"
                    );
                    checked += 1;
                }
                // And the other way: no producer codegen lowers is one the
                // core rule does not name.
                let mut whole = HashSet::new();
                for stmt in &method.expect("one method").body {
                    whole_values(&stmt.expression, &mut whole);
                }
                for (span, names) in &lowered {
                    assert!(
                        expected.contains_key(span) || whole.contains(span),
                        "codegen lowered the construct at {span:?} as a producer \
                         threading {names:?}, but the core rule names {expected:?}\n{src}"
                    );
                }
            }
        }
    }
    // Not vacuous: most fold/loop × position × context cells are producers.
    assert!(checked > 100, "only {checked} producer sites compared");
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2b (BT-3793): the loop-body half
// ---------------------------------------------------------------------------

/// The loop and fold frames a producer is nested in; `{s}` is the statement
/// under test. The last two put an arm and a closure between the frame and
/// the construct, where no site exists.
const FRAMES: &[&str] = &[
    "1 to: 2 do: [:k | {s}]",
    "#(1, 2) do: [:k | {s}]",
    "2 timesRepeat: [{s}]",
    "[s < 3] whileTrue: [s := s + 1. {s}]",
    "#(1, 2) collect: [:k | {s}. k]",
    "#(1, 2) inject: 0 into: [:a :k | {s}. a + k]",
    "#(1, 2) do: [:k | true ifTrue: [{s}]]",
    "#(1, 2) do: [:k | [:z | {s}] value: k]",
];

/// The statements placed in a frame body, the loop-body counterpart of
/// [`POSITIONS`].
const BODY_STATEMENTS: &[&str] = &[
    // The `Discard` tail: a construct as a body statement, bare and
    // parenthesised.
    "{c}",
    "({c})",
    // The `Rebind` tail: a construct as an assignment's value or operand.
    "r := {c}",
    "r := ({c})",
    "r := ({c}) size",
    "r := t + ({c})",
    "r := #[{c}, t]",
    "r := 0 max: ({c})",
    "Transcript show: ({c}) printString",
    // A producer in the receiver of another producer.
    "r := ({c}) inject: 0 into: [:a :x | u := u + 1. a + x]",
    // Sends whose statement lowering does not sequence its operands.
    "r := super respondsTo: ({c}) printString",
    "self error: ({c}) printString",
    "r := self perform: #size with: ({c})",
    "r := self.blk valueWithArguments: ({c})",
    "self.blk valueWithArguments: ({c})",
];

fn body_sources(frame: &str, construct: &str, statement: &str) -> [String; 3] {
    let inner = statement.replace("{c}", construct);
    let frame = frame.replace("{s}", &inner);
    let mut body = String::new();
    for line in format!("t := 0\nu := 0\ns := 0\nr := 0\n{frame}\nt").lines() {
        body.push_str("    ");
        body.push_str(line);
        body.push('\n');
    }
    [
        format!("Object subclass: AgreeClass\n  class go =>\n{body}"),
        format!("Value subclass: AgreeValue\n  go =>\n{body}"),
        format!(
            "Actor subclass: AgreeActor\n  state: n = 0\n  state: blk = nil\n\n  go =>\n{body}"
        ),
    ]
}

/// The core rule's verdict for every producer site of every loop or fold
/// body reachable from `stmt` (`loop_body_producer_sites`), frames nested in
/// one another included.
fn core_body_producers(stmt: &Expression, out: &mut HashMap<Span, Vec<String>>) {
    let bound = |name: &str| matches!(name, "t" | "u" | "s" | "r");
    let Expression::MessageSend { arguments, .. } = stmt.unwrap_parens() else {
        return;
    };
    for arg in arguments {
        let Expression::Block(block) = arg else {
            continue;
        };
        if !is_threaded_frame_body(stmt.unwrap_parens(), block) {
            continue;
        }
        for inner in &block.body {
            for site in loop_body_producer_sites(&inner.expression) {
                if let Some(writes) = operand_producer_writes(site, &bound) {
                    let mut names: Vec<String> =
                        writes.into_iter().map(|w| w.name.to_string()).collect();
                    names.sort();
                    names.dedup();
                    out.insert(Expression::span(site), names);
                    core_body_producers(site, out);
                }
            }
            core_body_producers(&inner.expression, out);
        }
    }
}

/// The producer half of ADR 0131 §6 for loop-body sites, both ways: a site
/// the core rule (`loop_body_producer_sites` + `operand_producer_writes`)
/// names is lowered by codegen's producer threading exactly its names, and
/// codegen lowers no producer at a site the rule does not name. A program
/// the Phase 0 checks reject is skipped.
#[test]
fn every_loop_body_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    for frame in FRAMES {
        for construct in CONSTRUCTS {
            for statement in BODY_STATEMENTS {
                for src in body_sources(frame, construct, statement) {
                    let (module, parse_diags) = parse(lex_with_eof(&src));
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
                        continue;
                    }
                    let class = &module.classes[0];
                    let method = class
                        .methods
                        .iter()
                        .chain(&class.class_methods)
                        .next()
                        .expect("one method");
                    let mut expected = core_producers(&method.body);
                    for stmt in &method.body {
                        core_body_producers(&stmt.expression, &mut expected);
                    }
                    let (generated, records) = recording_producers(|| {
                        generate_module_with_warnings(&module, CodegenOptions::new("agree"))
                    });
                    assert!(generated.is_ok(), "{src}: {:?}", generated.err());
                    let lowered: HashMap<Span, Vec<String>> = records
                        .into_iter()
                        .filter(|(_, names)| !names.is_empty())
                        .collect();
                    for (span, names) in &expected {
                        assert_eq!(
                            lowered.get(span),
                            Some(names),
                            "the core rule accepts the construct at {span:?} as a producer \
                             threading {names:?}, but codegen lowered {lowered:?}\n{src}"
                        );
                        checked += 1;
                    }
                    let mut whole = HashSet::new();
                    for stmt in &method.body {
                        whole_values(&stmt.expression, &mut whole);
                    }
                    for (span, names) in &lowered {
                        assert!(
                            expected.contains_key(span) || whole.contains(span),
                            "codegen lowered the construct at {span:?} as a producer \
                             threading {names:?}, but the core rule names {expected:?}\n{src}"
                        );
                    }
                }
            }
        }
    }
    assert!(checked > 100, "only {checked} loop-body sites compared");
}
