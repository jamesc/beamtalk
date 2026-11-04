// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4312: the `ensure:` relay over a generated corpus of scope kinds x
//! positions x `ensure:` shapes, through verified codegen and `erlc`.
//!
//! An `ensure:` is not a boundary: a `^` of the scope that unwinds through it
//! runs the cleanup, and the throw must resume with the cleanup's writes
//! (`ensure_nlr_relay`). The corpus crosses every scope kind a `^` can be
//! thrown from in a compiled class (an actor method, an actor closure, a
//! merged Tier 2 fun, a value-type method, a class method) with the
//! positions an `ensure:` sits in (a statement, a conditional arm, a loop
//! body, an `on:do:` protected block) and the `ensure:` shapes: both
//! operands block literals (the cleanup inlined), and a receiver or cleanup
//! that is not a block literal (the cleanup applied as a closure, the shape
//! of finding B of the BT-4312 review). Three proofs per cell:
//!
//! 1. It compiles with the `ThreadedIr` verifier on and through `erlc`.
//! 2. With the relay off (`fault::RelayFault::NoRelay`, every `ensure:`
//!    before BT-4312) it fails `EnsureCleanupLostOnNlr` exactly when its
//!    cleanup can write the state the `^` carries, by the oracle
//!    [`relay_needed`]: a closure cleanup in an actor method (it writes the
//!    cell) failed before the fix, with no relay.
//! 3. With the relay rebuilding a cell-carried throw's state at the
//!    `ensure:` site (`fault::RelayFault::RebuildAtSite`, the first BT-4312
//!    relay, finding A) every cell that relays in a scope whose state lives
//!    in the cell fails `RebuildsCarriedStateAtSite`, and no value-type or
//!    class cell does.
//!
//! The merged Tier 2 fun is reachable only in an unvalidated compile (ADR
//! 0133 rejects a stored block that assigns a captured local), like
//! `tier2_merge.rs`; codegen here never runs the validator. The runtime
//! answers are the `BUnit` cells of `stdlib/test/actor_closure_cell_test.bt`
//! (`ClosureCellEnsure`).

use super::{assert_compiles_through_erlc, erlc_available};
use crate::core_erlang::ensure_nlr_relay::fault::{RelayFault, with_fault};
use crate::core_erlang::{CodegenOptions, generate_module};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// A scope kind a `^` is thrown from, and the class it is compiled in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    ActorMethod,
    ActorClosure,
    MergedTier2Fun,
    ValueTypeMethod,
    ClassMethod,
}

const SCOPES: &[Scope] = &[
    Scope::ActorMethod,
    Scope::ActorClosure,
    Scope::MergedTier2Fun,
    Scope::ValueTypeMethod,
    Scope::ClassMethod,
];

impl Scope {
    /// The class header (one class per module) and the field the cleanups
    /// write.
    fn class(self) -> (&'static str, &'static str) {
        match self {
            Self::ActorMethod => ("Actor subclass: Bt4312RelayActor\n  state: n = 0\n", "n"),
            Self::ActorClosure => ("Actor subclass: Bt4312RelayClosure\n  state: n = 0\n", "n"),
            Self::MergedTier2Fun => ("Actor subclass: Bt4312RelayMerged\n  state: n = 0\n", "n"),
            Self::ValueTypeMethod => ("Value subclass: Bt4312RelayValue\n  field: k = 0\n", "k"),
            Self::ClassMethod => (
                "Object subclass: Bt4312RelayClass\n  classState: n = 0\n",
                "n",
            ),
        }
    }

    /// Method `name` running `stmt` in this scope.
    fn method(self, name: &str, stmt: &str) -> String {
        match self {
            Self::ActorMethod | Self::ValueTypeMethod => {
                format!("  {name} =>\n    {stmt}\n    0\n")
            }
            Self::ClassMethod => format!("  class {name} =>\n    {stmt}\n    0\n"),
            Self::ActorClosure => {
                format!("  {name} =>\n    b := [{stmt}. 0]\n    b value\n    0\n")
            }
            // BT-4122: a stored block that writes a captured local and a
            // field lowers as a merged Tier 2 fun; its `^` carries the cell
            // with the captured locals flushed over it.
            Self::MergedTier2Fun => format!(
                "  {name} =>\n    count := 0\n    b := [count := count + 1. self.n := self.n + 1. {stmt}. count]\n    b value\n    0\n"
            ),
        }
    }
}

/// Where the `ensure:` (`{E}`) sits.
const POSITIONS: &[(&str, &str)] = &[
    ("statement", "{E}"),
    ("arm", "true ifTrue: [{E}]"),
    ("loop body", "#(1, 2) do: [:i | {E}]"),
    ("protected block", "[{E}] on: Error do: [:e | 0]"),
];

/// How an `ensure:` shape's cleanup runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cleanup {
    /// Both operands are block literals: the cleanup is inlined. `writes`:
    /// it writes the field.
    Inlined { writes: bool },
    /// An operand is not a block literal: the cleanup is a closure.
    Closure,
}

/// The `ensure:` shapes, `{F}` a write of the scope's field. A shape with
/// two `ensure:`s is classified by the one that needs the most: a relay
/// failure of either fails the cell.
const SHAPES: &[(&str, &str, Cleanup)] = &[
    (
        "literal, writing",
        "[^1] ensure: [{F}]",
        Cleanup::Inlined { writes: true },
    ),
    (
        "literal, not writing",
        "[^1] ensure: [nil]",
        Cleanup::Inlined { writes: false },
    ),
    (
        "stored receiver",
        "r0 := [^1]. r0 ensure: [{F}]",
        Cleanup::Closure,
    ),
    (
        "stored cleanup",
        "c0 := [{F}]. [^1] ensure: c0",
        Cleanup::Closure,
    ),
    (
        "stored receiver and cleanup",
        "r0 := [^1]. c0 := [{F}]. r0 ensure: c0",
        Cleanup::Closure,
    ),
    (
        "stored cleanup, not writing",
        "c0 := [nil]. [^1] ensure: c0",
        Cleanup::Closure,
    ),
    (
        "stored cleanup inside a literal one",
        "c0 := [{F}]. [[^1] ensure: c0] ensure: [{F}]",
        Cleanup::Closure,
    ),
];

/// The oracle: whether the cleanup can write the state a `^` of `scope`
/// carries, as far as the `ensure:` site can tell, so the catch needs a
/// relay. A block literal that writes nothing never does. A closure the site
/// cannot see into may write the actor-state cell: the state of an actor
/// closure or merged Tier 2 fun, and the cell an actor method's `^` stored
/// its `State` to before it threw. A value type's `Self` is written only by
/// an inlined cleanup (a closure cannot rebind it, and one that assigns a
/// field is rejected), and a class method's throw carries nothing (its class
/// variables are written in place).
fn relay_needed(scope: Scope, cleanup: Cleanup) -> bool {
    match (scope, cleanup) {
        (_, Cleanup::Inlined { writes: false })
        | (Scope::ClassMethod, _)
        | (Scope::ValueTypeMethod, Cleanup::Closure) => false,
        (_, Cleanup::Inlined { writes: true } | Cleanup::Closure) => true,
    }
}

/// A value-type cell whose field write sits in a block compiled as a closure
/// (a stored block, or the cleanup of an `ensure:` whose receiver is one):
/// codegen rejects it, so it is no program and proves nothing.
fn rejected_value_type_closure(cell: &Cell, why: &str) -> bool {
    cell.scope == Scope::ValueTypeMethod && why.contains("Cannot assign to field 'k'")
}

struct Cell {
    scope: Scope,
    label: String,
    stmt: String,
    cleanup: Cleanup,
}

impl Cell {
    fn source(&self) -> String {
        let (header, _) = self.scope.class();
        format!("{header}\n{}", self.scope.method("probe", &self.stmt))
    }
}

fn cells() -> Vec<Cell> {
    let mut cells = Vec::new();
    for &scope in SCOPES {
        let (_, field) = scope.class();
        let write = format!("self.{field} := 9");
        for (position, template) in POSITIONS {
            for (shape, ensure, cleanup) in SHAPES {
                let stmt = template.replace("{E}", &ensure.replace("{F}", &write));
                cells.push(Cell {
                    scope,
                    label: format!("{scope:?} x {position} x {shape}"),
                    stmt,
                    cleanup: *cleanup,
                });
            }
        }
    }
    cells
}

fn panic_message(payload: &(dyn std::any::Any + Send)) -> String {
    payload
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
        .unwrap_or_else(|| "<non-string panic>".to_string())
}

/// Compiles `src` (a debug-build verifier finding is a panic): the code, or
/// why it fails.
fn compile(src: &str) -> Result<String, String> {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    if let Some(error) = diagnostics
        .iter()
        .find(|d| d.severity == beamtalk_core::source_analysis::Severity::Error)
    {
        return Err(format!("parse error: {}", error.message));
    }
    match catch_unwind(AssertUnwindSafe(|| {
        generate_module(&module, CodegenOptions::new("test"))
    })) {
        Ok(Ok(code)) => Ok(code),
        Ok(Err(e)) => Err(format!("codegen error: {e}")),
        Err(payload) => Err(format!("panic: {}", panic_message(payload.as_ref()))),
    }
}

/// The cells that are programs: every cell but a
/// [`rejected_value_type_closure`] one, which must be rejected for that
/// reason alone. Answers them, and the cells that failed otherwise.
fn compiled_cells() -> (Vec<(Cell, String)>, Vec<String>) {
    let mut compiled = Vec::new();
    let mut failures = Vec::new();
    for cell in cells() {
        match compile(&cell.source()) {
            Ok(code) => compiled.push((cell, code)),
            Err(why) if rejected_value_type_closure(&cell, &why) => {}
            Err(why) => failures.push(format!("{}: {why}\n{}", cell.label, cell.source())),
        }
    }
    (compiled, failures)
}

/// Proof 1: every cell compiles with the verifier on, with a relay exactly
/// where its oracle says the cleanup can write the carried state, and each
/// scope's cells, one method each, compile through `erlc` as one module.
#[test]
fn ensure_relay_corpus_compiles_and_relays() {
    let (compiled, mut failures) = compiled_cells();
    for (cell, code) in &compiled {
        let relays = code.contains("_EnsureNlrTok");
        let needed = relay_needed(cell.scope, cell.cleanup);
        if relays != needed {
            failures.push(format!(
                "{}: relays: {relays}, oracle: {needed}\n{}",
                cell.label,
                cell.source()
            ));
        }
    }
    for &scope in SCOPES {
        let in_scope = compiled.iter().filter(|(c, _)| c.scope == scope).count();
        // Every scope keeps its statement-position cells, at least.
        if in_scope < 3 {
            failures.push(format!("{scope:?}: only {in_scope} cell(s) compile"));
        }
    }
    if erlc_available() {
        for &scope in SCOPES {
            let (header, _) = scope.class();
            let methods: Vec<String> = compiled
                .iter()
                .filter(|(c, _)| c.scope == scope)
                .enumerate()
                .map(|(i, (c, _))| scope.method(&format!("probe{i}"), &c.stmt))
                .collect();
            let src = format!("{header}\n{}", methods.join("\n"));
            match compile(&src) {
                Ok(code) => {
                    if catch_unwind(AssertUnwindSafe(|| {
                        assert_compiles_through_erlc("test", &code);
                    }))
                    .is_err()
                    {
                        failures.push(format!("{scope:?}: erlc rejected the module"));
                    }
                }
                Err(why) => failures.push(format!("{scope:?} (batch): {why}")),
            }
        }
    }
    assert!(
        compiled.len() >= SCOPES.len() * POSITIONS.len() * SHAPES.len() * 4 / 5,
        "the corpus has thinned out: {}",
        compiled.len()
    );
    assert!(
        failures.is_empty(),
        "{} `ensure:` relay corpus cell(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// Proof 2: with the relay off, a cell fails `EnsureCleanupLostOnNlr`
/// exactly when its cleanup can write the carried state.
#[test]
fn ensure_relay_corpus_fails_without_the_relay() {
    let (compiled, mut failures) = compiled_cells();
    let mut fired = 0;
    for (cell, _) in &compiled {
        let src = cell.source();
        let report = with_fault(RelayFault::NoRelay, || compile(&src)).err();
        let lost = report
            .as_deref()
            .is_some_and(|r| r.contains("EnsureCleanupLostOnNlr") && r.contains("NoRelay"));
        let expected = relay_needed(cell.scope, cell.cleanup);
        fired += usize::from(lost);
        if lost != expected || (!expected && report.is_some()) {
            failures.push(format!(
                "{}: without the relay, expected EnsureCleanupLostOnNlr: {expected}, got {report:?}",
                cell.label
            ));
        }
    }
    assert!(fired >= 70, "too few cells need a relay: {fired}");
    assert!(
        failures.is_empty(),
        "{} cell(s) disagree:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// Proof 3 (finding A): with a cell-carried throw resumed with the state a
/// `^` at the `ensure:` site would carry, every cell of a scope whose state
/// lives in the cell (an actor closure, a merged Tier 2 fun) and that needs a
/// relay fails `RebuildsCarriedStateAtSite`, and no value-type or class
/// cell fails. An actor method's `ensure:` sits in a closure, whose state is
/// the cell, in some positions (an arm or a loop body that holds a `^`), so
/// its cells may fail too, but only that way.
#[test]
fn ensure_relay_corpus_fails_when_the_relay_rebuilds_at_the_site() {
    let (compiled, mut failures) = compiled_cells();
    let mut fired = 0;
    for (cell, _) in &compiled {
        let src = cell.source();
        let report = with_fault(RelayFault::RebuildAtSite, || compile(&src)).err();
        let rebuilt = report
            .as_deref()
            .is_some_and(|r| r.contains("RebuildsCarriedStateAtSite"));
        fired += usize::from(rebuilt);
        let needed = relay_needed(cell.scope, cell.cleanup);
        let agrees = match cell.scope {
            Scope::ActorClosure | Scope::MergedTier2Fun => rebuilt == needed,
            Scope::ActorMethod => rebuilt || report.is_none(),
            Scope::ValueTypeMethod | Scope::ClassMethod => report.is_none(),
        };
        if !agrees || (report.is_some() && !rebuilt) {
            failures.push(format!(
                "{}: rebuilding at the site, expected RebuildsCarriedStateAtSite: {needed}, got {report:?}",
                cell.label
            ));
        }
    }
    assert!(
        fired >= 40,
        "too few cells relay a cell-carried throw: {fired}"
    );
    assert!(
        failures.is_empty(),
        "{} cell(s) disagree:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
