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
    LoopBodyContext, is_actor_fold_value_site, is_fold_frame_body, is_loop_body_producer_site,
    is_threaded_frame_body, loop_body_producer_sites, operand_producer_writes,
    sequenced_operand_sites,
};
use beamtalk_core::semantic_analysis::{AnalysisContext, analyse_full};
use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, Span, lex_with_eof, parse};
use std::collections::{HashMap, HashSet};

pub(super) const CONSTRUCTS: &[&str] = &[
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
    // BT-3794: a handler that is the only writer (read-write and write-only),
    // two locals, and a parenthesised handler (a block value: rejected, so
    // skipped).
    "#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. 0]",
    "#(1, 2) detect: [:x | x > 1] ifNone: [t := 7. 0]",
    "#(1, 2) detect: [:x | x > 5] ifNone: [t := t + 1. u := u + 1. 0]",
    "#(1, 2) detect: [:x | x > 5] ifNone: ([t := t + 1. 0])",
    "#(1, 2) do: [:x | t := t + x]",
    // A map literal receiver goes through the dictionary `do:` lowering.
    "#{#a => 1, #b => 2} do: [:x | t := t + x]",
    "#(1, 2) anySatisfy: [:x | t := t + 1. false]",
    "#(1, 2) select: [:x | t := t + 1. true]",
    "#(1, 2) detect: [:x | t := t + 1. x > 1]",
    "[s < 2] whileFalse: [s := s + 1. t := t + 1]",
    "1 to: 5 by: 2 do: [:i | t := t + i]",
];

pub(super) const POSITIONS: &[&str] = &[
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

pub(super) fn sources(construct: &str, position: &str) -> [String; 3] {
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
pub(super) fn core_producers(
    body: &[beamtalk_core::ast::ExpressionStatement],
) -> HashMap<Span, Vec<String>> {
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
pub(super) fn whole_values(stmt: &Expression, out: &mut HashSet<Span>) {
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
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = value
    else {
        return;
    };
    // The receiver block of `[...] ensure:`/`on:do:`/`ifCurtailed:` is the
    // protected body, whose statements lower themselves like an arm's. Only
    // those selectors: a `whileTrue:` condition or `[...] value` receiver is
    // not a statement position.
    let protected_body = matches!(
        value,
        Expression::MessageSend { selector, .. }
            if matches!(selector.name().as_str(), "ensure:" | "on:do:" | "ifCurtailed:")
    );
    let receiver_block = protected_body.then_some(receiver.as_ref());
    for arg in receiver_block.into_iter().chain(arguments) {
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
                check_operand_agreement(&src, &mut checked);
            }
        }
    }
    // Not vacuous: most fold/loop × position × context cells are producers.
    assert!(checked > 100, "only {checked} producer sites compared");
}

/// The producer half of ADR 0131 §6 for the sequenced operand sites of one
/// program's method body, both ways: a site the core rule
/// (`sequenced_operand_sites` + `operand_producer_writes`) names is lowered by
/// codegen's producer threading exactly its names, and codegen lowers no
/// producer at a site the rule does not name. A program the Phase 0 checks
/// reject is skipped (nothing is compiled for it). `checked` counts the
/// producer sites compared.
fn check_operand_agreement(src: &str, checked: &mut usize) {
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
        return;
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
        *checked += 1;
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

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2d (BT-3795): the value of a field / class-variable write
// ---------------------------------------------------------------------------

/// Positions around a field or class-variable write `self.n := ...`. Only a
/// class method (a class-variable write) and an actor method (a field write)
/// have one: a value type cannot write `self.n :=` (ADR 0042), so the value
/// column is not compiled.
const FIELD_POSITIONS: &[&str] = &[
    "self.n := ({c}) size\n#[self.n, t]",
    "self.n := t + ({c})\n#[self.n, t]",
    "self.n := {c}\n#[self.n, t]",
    "self.n := ({c})\n#[self.n, t]",
    "self.n := #[{c}, t]\nself.n",
    "self.n := 0 max: ({c})\n#[self.n, t]",
    "self.n := ({c}) inject: 0 into: [:a :x | u := u + 1. a + x]\n#[self.n, t, u]",
    "self.n := ({c}) size\nself.n := self.n + ({c}) size\n#[self.n, t]",
    "self.n := ({c}) size",
    // A producer in the value of a write that is itself a `^` value, an
    // assignment's value, or nested in another write: not sites, so they
    // stay Phase 0 errors (skipped below) or compile with no producer.
    "^self.n := ({c}) size",
    "r := (self.n := ({c}) size)\n#[r, t]",
    "self.n := (self.n := ({c}) size)\n#[self.n, t]",
    // Sends whose statement lowering does not sequence its operands.
    "self.n := super respondsTo: ({c}) printString\nself.n",
    "self.n := self perform: #size with: ({c})\nself.n",
    "self.n := self.blk valueWithArguments: ({c})\nself.n",
    "self.n := self error: ({c}) printString\nt",
];

fn field_sources(construct: &str, position: &str) -> [String; 2] {
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
        format!(
            "Object subclass: AgreeClass\n  classState: n = 0\n  classState: blk = nil\n\n  class go =>\n{body}"
        ),
        format!(
            "Actor subclass: AgreeActor\n  state: n = 0\n  state: blk = nil\n\n  go =>\n{body}"
        ),
    ]
}

/// The value of a method-body field or class-variable write is a sequenced
/// operand site: a loop or fold in it that the core rule names is lowered by
/// codegen's producer, in both directions, in class and actor methods.
#[test]
fn every_field_write_value_site_the_core_rule_accepts_is_lowered_by_the_producer() {
    let mut checked = 0;
    let mut by_context = [0, 0];
    for construct in CONSTRUCTS {
        for position in FIELD_POSITIONS {
            for (i, src) in field_sources(construct, position).iter().enumerate() {
                let before = checked;
                check_operand_agreement(src, &mut checked);
                by_context[i] += checked - before;
            }
        }
    }
    assert!(
        by_context.iter().all(|&n| n > 50),
        "class and actor field-write sites compared: {by_context:?}"
    );
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2b (BT-3793): the loop-body half
// ---------------------------------------------------------------------------

/// The loop and fold frames a producer is nested in; `{s}` is the statement
/// under test. The two-level frames (BT-3796) put a letrec loop inside a fold
/// and a fold inside a letrec loop, where the innermost frame alone does not
/// tell whether an actor fold encloses the site. The last two put an arm and a
/// closure between the frame and the construct, where no site exists.
pub(super) const FRAMES: &[&str] = &[
    "1 to: 2 do: [:k | {s}]",
    "#(1, 2) do: [:k | {s}]",
    "2 timesRepeat: [{s}]",
    "[s < 3] whileTrue: [s := s + 1. {s}]",
    "#(1, 2) collect: [:k | {s}. k]",
    "#(1, 2) inject: 0 into: [:a :k | {s}. a + k]",
    "#(1, 2) do: [:k | 1 to: 2 do: [:i | {s}]]",
    "#(1, 2) collect: [:k | 2 timesRepeat: [{s}]. k]",
    "1 to: 2 do: [:k | #(1, 2) do: [:i | {s}]]",
    "[s < 3] whileTrue: [s := s + 1. #(1, 2) inject: 0 into: [:a :i | {s}. a + i]]",
    "#(1, 2) do: [:k | true ifTrue: [{s}]]",
    "#(1, 2) do: [:k | [:z | {s}] value: k]",
    // A letrec loop (or fold) nested in a fold through an arm or a closure:
    // the arm/closure sits between the two frames, so only the inner frame is
    // a frame site, yet an enclosing fold frame still exists for codegen.
    "#(1, 2) do: [:k | true ifTrue: [1 to: 2 do: [:i | {s}]]]",
    "#(1, 2) do: [:k | [:z | 1 to: 2 do: [:i | {s}]] value: k]",
    "#(1, 2) collect: [:k | true ifTrue: [2 timesRepeat: [{s}]]. k]",
    "#(1, 2) collect: [:k | [:z | 2 timesRepeat: [{s}]] value: k. k]",
    "#(1, 2) inject: 0 into: [:a :k | true ifTrue: [1 to: 2 do: [:i | {s}]]. a + k]",
    "#(1, 2) inject: 0 into: [:a :k | [:z | 1 to: 2 do: [:i | {s}]] value: k. a + k]",
    "2 timesRepeat: [true ifTrue: [#(1, 2) do: [:i | {s}]]]",
    "2 timesRepeat: [[:z | #(1, 2) do: [:i | {s}]] value: 1]",
    "#(1, 2) do: [:k | true ifTrue: [#(1, 2) inject: 0 into: [:a :i | {s}. a + i]]]",
    // A protected body or a handler between a fold and a nested loop.
    "#(1, 2) do: [:k | [1 to: 2 do: [:i | {s}]] ensure: [nil]]",
    "#(1, 2) do: [:k | [1 to: 2 do: [:i | {s}]] on: Error do: [:e | nil]]",
    "#(1, 2) do: [:k | [nil] on: Error do: [:e | 1 to: 2 do: [:i | {s}]]]",
    "#(1, 2) inject: 0 into: [:a :k | [2 timesRepeat: [{s}]] ensure: [nil]. a + k]",
];

/// The statements placed in a frame body, the loop-body counterpart of
/// [`POSITIONS`].
pub(super) const BODY_STATEMENTS: &[&str] = &[
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

pub(super) fn body_sources(frame: &str, construct: &str, statement: &str) -> [String; 3] {
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
/// body reachable from `stmt` (`loop_body_producer_sites`, narrowed by the
/// shared `is_loop_body_producer_site` exactly as the Phase 0 check and
/// codegen's `with_body_producer_sites` narrow it), frames nested in one
/// another included. `outer_fold` says whether a frame enclosing `stmt` is a
/// fold's; `value_bearing` collects the actor fold-frame value sites the
/// narrowing drops, which Phase 0 must have rejected.
pub(super) fn core_body_producers(
    stmt: &Expression,
    outer_fold: bool,
    in_actor: bool,
    out: &mut HashMap<Span, Vec<String>>,
    value_bearing: &mut HashSet<Span>,
) {
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
        let fold = is_fold_frame_body(stmt.unwrap_parens(), block);
        for inner in &block.body {
            let ctx = LoopBodyContext::new(&inner.expression, fold, outer_fold, in_actor);
            for site in loop_body_producer_sites(&inner.expression) {
                if let Some(writes) = operand_producer_writes(site, &bound) {
                    if is_actor_fold_value_site(site, &ctx) {
                        value_bearing.insert(Expression::span(site));
                    }
                    if !is_loop_body_producer_site(site, &ctx) {
                        continue;
                    }
                    let mut names: Vec<String> =
                        writes.into_iter().map(|w| w.name.to_string()).collect();
                    names.sort();
                    names.dedup();
                    out.insert(Expression::span(site), names);
                    core_body_producers(site, outer_fold || fold, in_actor, out, value_bearing);
                }
            }
            core_body_producers(
                &inner.expression,
                outer_fold || fold,
                in_actor,
                out,
                value_bearing,
            );
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
                    let mut value_bearing = HashSet::new();
                    let in_actor = src.starts_with("Actor");
                    for stmt in &method.body {
                        core_body_producers(
                            &stmt.expression,
                            false,
                            in_actor,
                            &mut expected,
                            &mut value_bearing,
                        );
                    }
                    // An actor fold frame (any enclosing one, BT-3796) lowers
                    // only the statement itself through the producer: Phase 0
                    // must reject every value-bearing site there (BT-3797,
                    // `r := xs inject: ...` in a `do:`, and the same site in a
                    // letrec nested in the fold). The narrowing is the shared
                    // `is_loop_body_producer_site`, which codegen's
                    // `with_body_producer_sites` reads too, so the lowered
                    // set compared below drops those sites as well.
                    assert!(
                        !src.starts_with("Actor") || value_bearing.is_empty(),
                        "Phase 0 accepted an actor fold-body value site {value_bearing:?}\n{src}"
                    );
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

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2c (BT-3794): statement-position `detect:ifNone:`
// ---------------------------------------------------------------------------

const DETECT_PREDICATES: &[&str] = &[
    "[:x | x > 5]",
    "[:x | t := t + 1. x > 5]",
    "([:x | x > 5])",
    "([:x | t := t + 1. x > 5])",
];

const DETECT_HANDLERS: &[&str] = &[
    "[t := t + 1. 0]",
    "[t := 7. 0]",
    "[u := 1. 0]",
    "([t := t + 1. 0])",
    "[0]",
];

const DETECT_STATEMENTS: &[&str] = &[
    "{c}\nt",
    "({c})\nt",
    "r := {c}\n#[r, t]",
    "r := ({c}) + t\n#[r, t]",
    "^({c}) + t",
    "{c}",
];

/// Every method-body `detect:ifNone:` send reachable as a statement, the
/// value of `x := V` / `^V`, or an operand site.
fn detect_sends(body: &[beamtalk_core::ast::ExpressionStatement]) -> Vec<&Expression> {
    let mut out = Vec::new();
    for stmt in body {
        let inner = stmt.expression.unwrap_parens();
        let value = match inner {
            Expression::Assignment { value, .. } | Expression::Return { value, .. } => {
                value.unwrap_parens()
            }
            other => other,
        };
        let mut candidates = vec![inner, value];
        candidates.extend(sequenced_operand_sites(&stmt.expression));
        for c in candidates {
            if matches!(c.unwrap_parens(), Expression::MessageSend { selector, .. }
                if selector.name() == "detect:ifNone:")
                && !out.iter().any(|o: &&Expression| o.span() == c.span())
            {
                out.push(c);
            }
        }
    }
    out
}

/// Both ways, for a writing `detect:ifNone:` anywhere a method-body statement
/// can hold one (the producer test above compares operand sites only): Phase
/// 0 accepts it (outside the REPL) exactly when core's producer rule names
/// it — a literal search block whose packed blocks cover every local it
/// writes — and codegen compiles every program Phase 0 accepts. Both
/// directions are asserted: accepted => named, and named => the check never
/// rejects the send itself.
#[test]
fn a_writing_detect_if_none_is_accepted_exactly_when_the_producer_rule_names_it() {
    let bound = |name: &str| matches!(name, "t" | "u" | "s" | "r");
    let (mut accepted, mut rejected_count) = (0, 0);
    for predicate in DETECT_PREDICATES {
        for handler in DETECT_HANDLERS {
            let construct = format!("#(1, 2) detect: {predicate} ifNone: {handler}");
            for position in DETECT_STATEMENTS {
                for src in sources(&construct, position) {
                    let (module, parse_diags) = parse(lex_with_eof(&src));
                    assert!(parse_diags.is_empty(), "{src}: {parse_diags:?}");
                    let analysis = analyse_full(&module, AnalysisContext::default());
                    let phase0_spans: Vec<Span> = analysis
                        .diagnostics
                        .iter()
                        .filter(|d| {
                            d.severity == Severity::Error
                                && matches!(
                                    d.category,
                                    Some(
                                        DiagnosticCategory::UnmigratedLocalThreading
                                            | DiagnosticCategory::Tier2BlockNoReturnChannel
                                    )
                                )
                        })
                        .map(|d| d.span)
                        .collect();
                    let rejected = !phase0_spans.is_empty();
                    let class = &module.classes[0];
                    let method = class
                        .methods
                        .iter()
                        .chain(&class.class_methods)
                        .next()
                        .expect("one method");
                    for send in detect_sends(&method.body) {
                        let writes = beamtalk_core::semantic_analysis::block_facts::local_threading_construct(send)
                            .map(|c| {
                                beamtalk_core::semantic_analysis::block_facts::construct_outer_local_writes(&c, &bound)
                            })
                            .unwrap_or_default();
                        if writes.is_empty() {
                            continue;
                        }
                        let named = operand_producer_writes(send, &bound).is_some();
                        // And the converse (named => accepted): the check
                        // never rejects the send itself when the rule names it.
                        assert!(
                            !(named && phase0_spans.contains(&send.span())),
                            "Phase 0 rejects a writing `detect:ifNone:` the producer rule \
                             names\n{src}"
                        );
                        if rejected {
                            rejected_count += 1;
                        } else {
                            assert!(
                                named,
                                "Phase 0 accepted a writing `detect:ifNone:` the producer rule \
                                 does not name (codegen would drop its write)\n{src}"
                            );
                            accepted += 1;
                        }
                    }
                    if !rejected {
                        let generated =
                            generate_module_with_warnings(&module, CodegenOptions::new("agree"));
                        assert!(generated.is_ok(), "{src}: {:?}", generated.err());
                    }
                }
            }
        }
    }
    assert!(
        accepted > 50 && rejected_count > 50,
        "{accepted} accepted, {rejected_count} rejected"
    );
}
