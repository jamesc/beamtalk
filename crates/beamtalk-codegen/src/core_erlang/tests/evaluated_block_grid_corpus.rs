// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4274: the generated BT-3981 evaluated-block grid, hosts x positions x
//! evaluated blocks x contexts, through verified codegen and `erlc`.
//!
//! BT-3982 made the actor evaluated literal block a `ThreadedIr` producer
//! wherever it sits (`evaluated_block_producer.rs`). The grid is generated once,
//! by `beamtalk_core::test_helpers::evaluated_block_grid`; the ADR 0131
//! validator accepts every cell (`beamtalk-core`'s
//! `evaluated_block_value_corpus_is_accepted_everywhere`), and here every
//! actor, class and value-type cell must pass the `ThreadedIr` verifier (a
//! panic in a debug build, an `internal:` diagnostic in a release one) and
//! compile through `erlc`. The runtime answers are the `BUnit` fixtures
//! (`stdlib/test/bt3981evalblock_test.bt`, `bt3982evalblock_test.bt`).

use super::{assert_compiles_through_erlc, erlc_available};
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};
use crate::core_erlang::{CodegenOptions, generate_module_surfacing_verifier};
use beamtalk_core::test_helpers::evaluated_block_grid::{self as grid, CONTEXTS, Cell, HOSTS};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// One family per host of [`HOSTS`] (`per_family_tests!`, BT-3888): each is
/// one `erlc` run per context over every position x block of the host.
const HOST_FAMILIES: &[std::ops::Range<usize>] = &[0..1, 1..2, 2..3, 3..4, 4..5, 5..6, 6..7];

/// PIN-BUG list: cell labels ([`Cell::label`]) that fail today, each citing an
/// open issue. The corpus asserts the failing set EQUALS the pins of its
/// family, so a fix flips every pin it covers. Empty: every cell compiles.
const PINNED_FAILURES: &[&str] = &[];

/// Compiles the probe class of `src` (a [`grid::grid_source`] in `context`;
/// one class per module, as the compiler requires) through the verifier and
/// `erlc`, answering why it fails. The generated module must describe every
/// one of `probes` (its `method_info` / `class_method_info` entry), so a
/// source whose probe class was lost cannot pass vacuously.
fn compile_failure(context: grid::Context, src: &str, probes: &[&str]) -> Option<String> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (mut module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    if let Some(error) = diagnostics
        .iter()
        .find(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
    {
        return Some(format!("parse error: {}", error.message));
    }
    module
        .classes
        .retain(|c| c.name.name == context.probe_class());
    let generated = catch_unwind(AssertUnwindSafe(|| {
        generate_module_surfacing_verifier(&module, CodegenOptions::new("test"))
    }));
    match generated {
        Err(_) => Some("debug ThreadedIr verifier / codegen panic".to_string()),
        Ok(Err(e)) => Some(format!("codegen error: {e}")),
        Ok(Ok((_, verifier))) if !verifier.is_empty() => Some(format!(
            "ThreadedIr verifier diagnostic(s): {}",
            verifier
                .iter()
                .map(|d| d.message.to_string())
                .collect::<Vec<_>>()
                .join("; ")
        )),
        Ok(Ok((code, _))) => {
            if let Some(missing) = probes
                .iter()
                .find(|p| !code.contains(&format!("'{p}' => ~{{'arity' => 0")))
            {
                return Some(format!("no method `{missing}` in the generated module"));
            }
            catch_unwind(AssertUnwindSafe(|| {
                assert_compiles_through_erlc("test", &code);
            }))
            .err()
            .map(|_| "erlc rejected the generated Core Erlang".to_string())
        }
    }
}

/// How a module is compiled: [`compile_failure`]'s signature, a parameter so
/// a test can inject a failure that only the batched module shows.
type Compile = dyn Fn(grid::Context, &str, &[&str]) -> Option<String>;

/// The cells of `cells` in `context` as one module, each its own probe method
/// (`probe0`, `probe1`, ...), through `compile`: one `erlc` run.
fn compile_batch_failure(
    cells: &[Cell],
    context: grid::Context,
    compile: &Compile,
) -> Option<String> {
    let probes: Vec<(String, String)> = cells
        .iter()
        .filter(|c| c.context == context)
        .enumerate()
        .map(|(i, cell)| (format!("probe{i}"), cell.body()))
        .collect();
    let names: Vec<&str> = probes.iter().map(|(name, _)| name.as_str()).collect();
    compile(context, &grid::grid_source(context, &probes), &names)
}

/// The failures of `cells` under `compile`: per context, the batched module
/// first, and each cell on its own only when the batch fails, so a failure
/// names its cell. A batch failure no single cell explains (a collision
/// between methods of one module: fresh temps, fun names, `method_info`,
/// duplicate letrec names) is a failure of its own, labelled `batch
/// (<context>)`, which names no pin.
fn grid_failures(cells: &[Cell], compile: &Compile) -> Vec<String> {
    let mut failures: Vec<String> = Vec::new();
    for &context in CONTEXTS {
        let Some(batch) = compile_batch_failure(cells, context, compile) else {
            continue;
        };
        let before = failures.len();
        for cell in cells.iter().filter(|c| c.context == context) {
            let src = cell.source();
            if let Some(why) = compile(context, &src, &["probe"]) {
                failures.push(format!("{}\n  {why}\n{src}", cell.label()));
            }
        }
        if failures.len() == before {
            failures.push(format!(
                "batch ({context:?}): no single cell fails\n  {batch}"
            ));
        }
    }
    failures
}

/// Every cell of the grid whose host is in `hosts`, in every context
/// ([`grid_failures`]), compiles, or fails exactly as pinned.
fn evaluated_block_grid_corpus(hosts: &[&'static str]) {
    let failures = grid_failures(&grid::cells_of(hosts), &compile_failure);
    let failing: Vec<&str> = failures
        .iter()
        .map(|f| f.lines().next().unwrap_or_default())
        .collect();
    let pinned: Vec<&str> = PINNED_FAILURES
        .iter()
        .copied()
        .filter(|p| hosts.iter().any(|h| p.starts_with(&format!("{h} x "))))
        .collect();
    let unpinned: Vec<&String> = failures
        .iter()
        .filter(|f| !pinned.contains(&f.lines().next().unwrap_or_default()))
        .collect();
    assert!(
        unpinned.is_empty(),
        "{} evaluated-block grid cell(s) fail verified codegen:\n\n{}",
        unpinned.len(),
        unpinned
            .iter()
            .map(|f| f.as_str())
            .collect::<Vec<_>>()
            .join("\n\n")
    );
    // A pin that only fails at `erlc` cannot be checked without it.
    if erlc_available() {
        let flipped: Vec<&&str> = pinned.iter().filter(|p| !failing.contains(p)).collect();
        assert!(
            flipped.is_empty(),
            "pinned cell(s) now compile; remove them from PINNED_FAILURES: {flipped:?}"
        );
    }
}

beamtalk_core::per_family_tests! {
    list = HOSTS,
    families = HOST_FAMILIES,
    check = evaluated_block_grid_corpus,
    partition = the_evaluated_block_grid_corpus_tests_cover_every_host,
    evaluated_block_grid_corpus_method_host => 0,
    evaluated_block_grid_corpus_arm_host => 1,
    evaluated_block_grid_corpus_collect_host => 2,
    evaluated_block_grid_corpus_protected_host => 3,
    evaluated_block_grid_corpus_stored_host => 4,
    evaluated_block_grid_corpus_ffi_host => 5,
    evaluated_block_grid_corpus_user_host => 6,
}

/// Every pin of [`PINNED_FAILURES`] names a cell of the grid: a pin for a cell
/// no family holds would never be checked, so it could neither fail nor flip.
#[test]
fn every_evaluated_block_grid_corpus_pin_names_a_cell() {
    let labels: Vec<String> = grid::cells().iter().map(Cell::label).collect();
    for pin in PINNED_FAILURES {
        assert!(labels.iter().any(|l| l == pin), "unknown pin {pin:?}");
    }
}

/// The corpus is not vacuous: with the BT-3982 producer gate disabled
/// ([`LocalRebindFault::EvaluatedBlockNotAProducer`], the lowering before the
/// fix), actor cells of every `State`-threading host (`method`, `arm`,
/// `collect`, `protected`, the hosts BT-3981 measured leaking the carrier)
/// fail verified codegen, and no class or value-type cell does (the fault
/// gates the actor producer only). One module per host and context, as in the
/// corpus.
#[test]
fn the_evaluated_block_grid_corpus_fails_without_the_bt3982_producer() {
    for host in ["method", "arm", "collect", "protected"] {
        let cells = grid::cells_of(&[host]);
        for &context in CONTEXTS {
            let failure = fault::with(LocalRebindFault::EvaluatedBlockNotAProducer, || {
                compile_batch_failure(&cells, context, &compile_failure)
            });
            if context == grid::Context::Actor {
                assert!(failure.is_some(), "{host}: the actor cells compile");
            } else {
                assert_eq!(failure, None, "{host} ({context:?})");
            }
        }
    }
}

/// A failure only the batched module shows (here injected: any module with
/// more than one probe fails) is reported, not dropped because every cell
/// compiles on its own.
#[test]
fn a_batch_only_evaluated_block_grid_corpus_failure_is_reported() {
    let batch_only = |_: grid::Context, _: &str, probes: &[&str]| {
        (probes.len() > 1).then(|| "injected batch-only failure".to_string())
    };
    let failures = grid_failures(&grid::cells_of(&["method"]), &batch_only);
    let labels: Vec<&str> = failures
        .iter()
        .map(|f| f.lines().next().unwrap_or_default())
        .collect();
    assert_eq!(
        labels,
        [
            "batch (Actor): no single cell fails",
            "batch (Class): no single cell fails",
            "batch (Value): no single cell fails",
        ]
    );
    assert!(labels.iter().all(|l| !PINNED_FAILURES.contains(l)));
}
