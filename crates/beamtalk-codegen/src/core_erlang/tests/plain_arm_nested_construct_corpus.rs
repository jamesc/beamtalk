// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4460 (ADR 0131 §2): a nested inline construct (a conditional, `ifNil:`,
//! `and:`/`or:`, `match:`, `on:do:`/`ensure:`) that writes a local of a
//! *plain* arm (the arm of a conditional that writes no outer local,
//! `generate_plain_arm`) is a producer of that arm frame: its `ConstructTuple`
//! and one `LocalRebind` per local land in the arm. Before the fix a plain arm
//! registered only loop and fold sites, so in an actor method such a construct
//! was compiled in place and the arm reread its locals out of the construct's
//! state carrier (the legacy write-back BT-4462 deleted).
//! `VerifyError::ConstructLocalsNotRebound` reports such a construct.
//!
//! The corpus is the generated `BUnit` grid's own fixtures
//! (`scripts/gen-bt4460-plain-arm-nested-grid.py`): nested selector x
//! statement position x write placement x writer, in actor, value-type and
//! `Object` class-side methods. Each method is compiled on its own: every one
//! is clean, and with the plain arm's producer sites withheld
//! (`PlainArmOpaqueProducers`) every actor cell that writes the arm-local as a
//! statement reports the construct compiled in place.

use super::verifier_report;
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

const ACTOR: &str =
    include_str!("../../../../../stdlib/test/fixtures/bt4460plain_arm_nested_actor.bt");
const VALUE: &str =
    include_str!("../../../../../stdlib/test/fixtures/bt4460plain_arm_nested_value.bt");
const OBJECT: &str =
    include_str!("../../../../../stdlib/test/fixtures/bt4460plain_arm_nested_object.bt");

/// One method of a grid fixture, as a program of its own: the fixture's class
/// header (its `subclass:` line and slot) followed by the method.
struct Cell {
    name: String,
    program: String,
}

/// The fixture's methods, each with the class header.
fn cells(fixture: &str) -> Vec<Cell> {
    let lines: Vec<&str> = fixture.lines().collect();
    let class_line = lines
        .iter()
        .position(|l| l.contains(" subclass: "))
        .expect("a class definition");
    let mut header = vec![lines[class_line]];
    let mut i = class_line + 1;
    while i < lines.len() && !lines[i].trim().is_empty() {
        header.push(lines[i]);
        i += 1;
    }
    let mut out = Vec::new();
    let mut current: Vec<&str> = Vec::new();
    for line in &lines[i..] {
        if line.trim().is_empty() {
            if !current.is_empty() {
                out.push(std::mem::take(&mut current));
            }
        } else {
            current.push(line);
        }
    }
    if !current.is_empty() {
        out.push(current);
    }
    out.into_iter()
        .map(|method| {
            let name = method[0]
                .trim()
                .trim_start_matches("class ")
                .split([' ', ':'])
                .next()
                .unwrap_or_default()
                .to_string();
            let mut program = header.join("\n");
            program.push('\n');
            program.push_str(&method.join("\n"));
            program.push('\n');
            Cell { name, program }
        })
        .collect()
}

#[test]
fn the_corpus_holds_every_kind() {
    for (kind, fixture) in [("actor", ACTOR), ("value", VALUE), ("object", OBJECT)] {
        let n = cells(fixture).len();
        assert!(n >= 150, "{kind}: only {n} methods in the grid fixture");
    }
}

/// The issue's repro: an actor method whose plain arm holds, as a non-last
/// statement, a nested conditional that writes the arm-local `m`.
fn repro() -> &'static str {
    "Actor subclass: Probe
  state: n = 0

  run: c =>
    r := c ifTrue: [m := 0. c ifTrue: [m := 5. 7] ifFalse: [0]. m] ifFalse: [0]
    r
"
}

#[test]
fn the_repro_passes_the_verifier() {
    assert_eq!(verifier_report(repro(), None), None);
}

#[test]
fn the_pre_bt4460_arm_rule_compiles_the_repro_in_place() {
    let report = verifier_report(repro(), Some(LocalRebindFault::ArmLoopOrFoldSitesOnly))
        .expect("the in-place construct is a verifier finding");
    assert!(
        report.contains("ConstructLocalsNotRebound") && report.contains("\"m\""),
        "expected ConstructLocalsNotRebound for `m`, got: {report}"
    );
}

/// Every method of every grid fixture compiles clean: no construct writing a local is compiled in place
/// and every arm's producer sites are threaded.
#[test]
fn every_grid_method_compiles_clean() {
    let mut failures = Vec::new();
    let mut compiled = 0;
    for (kind, fixture) in [("actor", ACTOR), ("value", VALUE), ("object", OBJECT)] {
        for cell in cells(fixture) {
            compiled += 1;
            if let Some(report) = verifier_report(&cell.program, None) {
                failures.push(format!("{kind} {}: {report}", cell.name));
            }
        }
    }
    assert!(compiled >= 700, "only {compiled} grid methods compiled");
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// The grid's nested selectors whose receiver is the plain variable `c` or
/// `z`, or a block literal: as a non-last statement of a plain arm, the
/// pre-BT-4460 arm compiled them in place (C10). The
/// others (`ifFalse:` and `or:` on `c not`, a `match:`) were lowered another
/// way before BT-4460 too and never reached C10.
const IN_PLACE_BEFORE_BT4460: &[&str] = &[
    "ifTrue_ifFalse_",
    "ifTrue_",
    "ifNil_ifNotNil_",
    "ifNil_",
    "ifNotNil_",
    "and_",
    "on_do_",
    "ensure_",
];

/// With the plain-arm rule before BT-4460 restored (a plain arm registers and
/// checks only its loop and fold sites), every actor cell whose nested
/// construct writes the arm-local as a non-last statement of the plain arm
/// is compiled in place ([`IN_PLACE_BEFORE_BT4460`]), and the verifier reports
/// it. As an assignment's value or the arm's last expression the construct
/// never reached it. Nothing else is reported.
#[test]
fn the_pre_bt4460_arm_rule_is_reported_over_the_corpus() {
    let mut reported = 0;
    let mut missed = Vec::new();
    for cell in cells(ACTOR).into_iter().filter(|c| c.name.ends_with("Arm")) {
        let expected = cell.name.contains("_nonlast_")
            && IN_PLACE_BEFORE_BT4460.iter().any(|selector| {
                cell.name
                    .strip_prefix(selector)
                    .is_some_and(|rest| rest.starts_with("nonlast_"))
            });
        match verifier_report(
            &cell.program,
            Some(LocalRebindFault::ArmLoopOrFoldSitesOnly),
        ) {
            Some(report) => {
                assert!(
                    report.contains("ConstructLocalsNotRebound"),
                    "unexpected finding for {}: {report}",
                    cell.name
                );
                assert!(
                    expected,
                    "{} reported an in-place construct: {report}",
                    cell.name
                );
                reported += 1;
            }
            None if expected => missed.push(cell.name),
            None => {}
        }
    }
    assert!(
        missed.is_empty(),
        "{} cells did not report the in-place construct: {}",
        missed.len(),
        missed.join(", ")
    );
    assert!(
        reported >= 40,
        "only {reported} cells reported the in-place construct"
    );
}
