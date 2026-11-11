// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4461 (ADR 0131 §2, the `MethodBody (REPL)` row): a REPL frame other than
//! the root (a conditional arm, an `on:do:`/`ensure:` block, an evaluated
//! block, a loop or fold body) holds the bindings it writes in Core variables.
//! A construct at one of its producer sites that writes such a binding adopts
//! the bindings map whole, so it must thread the binding: its
//! `ConstructTuple` lists it and one `LocalRebind` reads it back out of the
//! adopted map. Before the fix no REPL producer did, so the frame read the
//! value from before the construct, and a loop of the construct read its
//! binding from the frame's variable and never ended;
//! `VerifyError::ReplCachedBindingStale` now reports that.
//!
//! The corpus is the generated REPL-protocol grid's own inputs
//! (`scripts/gen-bt4461-repl-arm-cached-binding-grid.py`): outer frame x nested
//! writer x statement position x write placement. Each input is compiled as the
//! REPL compiles it: every one is clean, and with the producers' rethreading
//! withheld (`ReplCachedBindingsNotRethreaded`) every cell whose frame wrote
//! the binding first reports the stale binding, and no other cell does.

use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};
use crate::core_erlang::{CodeGenContext, CodeGenError, CoreErlangGenerator};
use beamtalk_core::ast::Expression;

const GRID: &str = include_str!(
    "../../../../../tests/repl-protocol/cases/bt4461_repl_arm_cached_binding_grid.btscript"
);

/// One cell of the grid: its header (`outer / writer / position /
/// placement`) and its input.
struct Cell {
    name: String,
    pre: bool,
    input: String,
}

/// The grid's cells, each with the `r := ...` input its header introduces.
fn cells() -> Vec<Cell> {
    let mut out = Vec::new();
    let mut header: Option<String> = None;
    for line in GRID.lines() {
        if let Some(rest) = line.strip_prefix("// ")
            && rest.split(" / ").count() == 4
        {
            header = Some(rest.to_string());
        } else if (line.starts_with("r := ") || line.starts_with("#("))
            && let Some(name) = header.take()
        {
            out.push(Cell {
                pre: name.ends_with("/ pre"),
                name,
                input: line.to_string(),
            });
        }
    }
    out
}

/// Compiles `input` as the REPL compiles one input, under `fault`; answers the
/// verifier's finding (the debug-build panic message), if any.
fn repl_verifier_report(input: &str, fault: Option<LocalRebindFault>) -> Option<String> {
    let src = format!("{input}\n");
    let (module, diagnostics) = super::parse_normalised(&src);
    assert!(diagnostics.is_empty(), "{src}: {diagnostics:?}");
    let statements: Vec<&Expression> = module.expressions.iter().map(|s| &s.expression).collect();
    let compile = || {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let mut generator = CoreErlangGenerator::new("bt4461_repl");
            generator.context = CodeGenContext::Repl;
            generator.set_is_repl_mode(true);
            generator.push_scope();
            generator.bind_var("__bindings__", "Bindings");
            for stmt in &statements {
                if generator
                    .repl_statement_with_producers(stmt, stmt)?
                    .is_none()
                {
                    generator.expression_doc_with_repl_mutation_tracking(stmt)?;
                }
            }
            Ok::<(), CodeGenError>(())
        }))
    };
    let result = match fault {
        Some(fault) => fault::with(fault, compile),
        None => compile(),
    };
    match result {
        Ok(Ok(())) => None,
        Ok(Err(e)) => Some(format!("codegen error: {e:?}")),
        Err(payload) => Some(
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default(),
        ),
    }
}

/// The issue's repros: a nested conditional, and a loop, writing the binding
/// `m` the arm wrote first.
const REPROS: &[&str] = &[
    "r := c ifTrue: [m := 1. c ifTrue: [m := 5] ifFalse: [nil]. m] ifFalse: [0]",
    "r := c ifTrue: [m := 1. [m < 5] whileTrue: [m := m + 1]. m] ifFalse: [0]",
];

#[test]
fn the_grid_holds_every_frame_and_writer() {
    let cells = cells();
    assert!(cells.len() >= 900, "only {} grid cells", cells.len());
    assert!(
        cells.iter().any(|c| !c.pre),
        "no cell without the first write"
    );
}

#[test]
fn the_repros_pass_the_verifier() {
    for repro in REPROS {
        assert_eq!(repl_verifier_report(repro, None), None, "{repro}");
    }
}

#[test]
fn the_pre_bt4461_repl_producers_leave_the_repros_stale() {
    for repro in REPROS {
        let report = repl_verifier_report(
            repro,
            Some(LocalRebindFault::ReplCachedBindingsNotRethreaded),
        )
        .unwrap_or_else(|| panic!("no verifier finding for {repro}"));
        assert!(
            report.contains("ReplCachedBindingStale") && report.contains("\"m\""),
            "expected ReplCachedBindingStale for `m` in {repro}, got: {report}"
        );
    }
}

/// Every input of the grid compiles clean: every producer that writes a
/// binding its frame holds in a variable threads it.
#[test]
fn every_grid_input_compiles_clean() {
    let failures: Vec<String> = cells()
        .into_iter()
        .filter_map(|cell| {
            repl_verifier_report(&cell.input, None)
                .map(|report| format!("{}: {}\n  {report}", cell.name, cell.input))
        })
        .collect();
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

/// With the REPL producers' rethreading withheld (the lowering before
/// BT-4461), every cell whose frame wrote `m` before the nested writer reports
/// `ReplCachedBindingStale`, whatever the frame, writer and position; a cell
/// whose frame never wrote `m` holds no variable for it and reports nothing.
#[test]
fn the_pre_bt4461_repl_producers_are_reported_over_the_grid() {
    let mut reported = 0;
    let mut wrong = Vec::new();
    for cell in cells() {
        let report = repl_verifier_report(
            &cell.input,
            Some(LocalRebindFault::ReplCachedBindingsNotRethreaded),
        );
        match (cell.pre, report) {
            (true, Some(report)) if report.contains("ReplCachedBindingStale") => reported += 1,
            (false, None) => {}
            (_, report) => wrong.push(format!("{}: {report:?}", cell.name)),
        }
    }
    assert!(
        wrong.is_empty(),
        "{} cells disagree:\n{}",
        wrong.len(),
        wrong.join("\n")
    );
    assert!(
        reported >= 850,
        "only {reported} cells reported the stale binding"
    );
}

/// BT-4499: the issue's repros, a nested construct writing an enclosing block
/// parameter. The frame binds the parameter to a variable that is not a key of
/// the bindings map, so the construct's write was dropped (or its loop never
/// ended) until the parameter was seeded into the map for the construct.
const PARAMETER_REPROS: &[&str] = &[
    "r := #(1, 2) inject: 0 into: [:acc :x | 1 to: x do: [:k | acc := acc + k]. acc]",
    "r := #(1, 7) inject: 0 into: [:a :i | (i > 5) ifTrue: [i := 0] ifFalse: [nil]. a + i]",
    "r := #(1, 5) inject: 0 into: [:a :i | [i < 3] whileTrue: [i := i + 1]. a + i]",
    "r := 7 ifNotNil: [:v | [v < 9] whileTrue: [v := v + 1]. v]",
];

#[test]
fn the_parameter_repros_pass_the_verifier() {
    for repro in PARAMETER_REPROS {
        assert_eq!(repl_verifier_report(repro, None), None, "{repro}");
    }
}

/// With the parameter left out of the bindings map (`ReplParamsNotSeeded`) the
/// construct threads nothing for it, and `ReplCachedBindingStale` names the
/// parameter.
#[test]
fn an_unseeded_parameter_is_reported_stale() {
    for repro in PARAMETER_REPROS {
        let report = repl_verifier_report(repro, Some(LocalRebindFault::ReplParamsNotSeeded))
            .unwrap_or_else(|| panic!("no verifier finding for {repro}"));
        assert!(
            report.contains("ReplCachedBindingStale"),
            "expected ReplCachedBindingStale in {repro}, got: {report}"
        );
    }
}
