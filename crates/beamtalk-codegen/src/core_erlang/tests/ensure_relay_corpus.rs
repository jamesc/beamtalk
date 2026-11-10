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
//! of finding B of the BT-4312 review), and who throws the `^` (ADR 0146
//! R1): the scope itself, or another method of the same class that passes
//! the `ensure:`'s method a `^` block (a foreign `^` there). Four proofs per
//! cell:
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
//! 4. With no foreign clause (`fault::RelayFault::ForeignOnErrorPath`, every
//!    `ensure:` before ADR 0146 R1) a cell fails
//!    `EnsureForeignCleanupNotStored` exactly when, by the oracle
//!    [`foreign_store_needed`], its `ensure:` sits where the actor-state cell
//!    is live with an inlined cleanup that writes: every such foreign-throw
//!    cell fails, so a foreign `^` there would take the error path and drop
//!    the cleanup's writes.
//!
//! A fifth proof bounds code size (BT-4598): a cleanup nested `depth`
//! `ensure:`s deep in an actor method is compiled exactly 3^depth times, the
//! relay and the foreign clause sharing one body.
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
    /// write. Each class has a `bump` the indirect shapes send to `self`:
    /// it writes the field where the scope can (BT-4433).
    fn class(self) -> (&'static str, &'static str) {
        match self {
            Self::ActorMethod => (
                "Actor subclass: Bt4312RelayActor\n  state: n = 0\n  bump => self.n := self.n + 1\n",
                "n",
            ),
            Self::ActorClosure => (
                "Actor subclass: Bt4312RelayClosure\n  state: n = 0\n  bump => self.n := self.n + 1\n",
                "n",
            ),
            Self::MergedTier2Fun => (
                "Actor subclass: Bt4312RelayMerged\n  state: n = 0\n  bump => self.n := self.n + 1\n",
                "n",
            ),
            // A value type's `bump` answers a new value; it cannot rebind the
            // sender's `Self`.
            Self::ValueTypeMethod => (
                "Value subclass: Bt4312RelayValue\n  field: k = 0\n  bump => self withK: self.k + 1\n",
                "k",
            ),
            Self::ClassMethod => (
                "Object subclass: Bt4312RelayClass\n  classState: n = 0\n  class bump => self.n := self.n + 1\n",
                "n",
            ),
        }
    }

    /// Method `name` (a unary selector, or a keyword one with its
    /// parameter) running `stmt` in this scope.
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

/// Who throws the `^` that unwinds through the `ensure:` (ADR 0146 R1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Thrower {
    /// The scope itself: `[^1]` is its own block.
    OwnScope,
    /// Another method of the same class: it sends the `ensure:`'s method a
    /// `^` block (`self probe: [^1]`), which that method runs as `blk value`
    /// (or holds as `r0 := blk`). Its `^` is foreign at the `ensure:`.
    OtherMethod,
}

const THROWERS: &[Thrower] = &[Thrower::OwnScope, Thrower::OtherMethod];

impl Thrower {
    /// `ensure`, with the scope's own `^` block replaced by the method's
    /// parameter where another method throws.
    fn ensure(self, ensure: &str) -> String {
        match self {
            Self::OwnScope => ensure.to_string(),
            Self::OtherMethod => ensure
                .replace("r0 := [^1]", "r0 := blk")
                .replace("[^1]", "[blk value]"),
        }
    }

    /// The methods of a cell named `name` running `stmt` in `scope`: the
    /// probe, and for another method's `^` the method that sends it one.
    fn methods(self, scope: Scope, name: &str, stmt: &str) -> String {
        match self {
            Self::OwnScope => scope.method(name, stmt),
            Self::OtherMethod => format!(
                "{}\n{}",
                scope.method(&format!("{name}: blk"), stmt),
                scope.method(&format!("call{name}"), &format!("self {name}: [^1]"))
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
    /// BT-4433: both operands are block literals and the cleanup writes the
    /// field only indirectly, through a self-send (`self bump`) or a stored
    /// block it runs (`c0 value`). Which of these the cleanup's
    /// `writes_cell` sees is the `block_touches_actor_state` heuristic's
    /// call; the oracle says what the program does.
    Indirect,
}

/// An `ensure:` shape: its label, its source (`{F}` a write of the scope's
/// field), how its cleanup runs, and whether it holds an inlined cleanup
/// that writes the scope's state (both operands block literals, the cleanup
/// writing directly or indirectly), the one a foreign `^` must find stored
/// in the cell (ADR 0146 R1). A shape with two `ensure:`s is classified by
/// the one that needs the most: a relay failure of either fails the cell.
struct Shape {
    label: &'static str,
    ensure: &'static str,
    cleanup: Cleanup,
    inlined_write: bool,
}

const SHAPES: &[Shape] = &[
    Shape {
        label: "literal, writing",
        ensure: "[^1] ensure: [{F}]",
        cleanup: Cleanup::Inlined { writes: true },
        inlined_write: true,
    },
    Shape {
        label: "literal, not writing",
        ensure: "[^1] ensure: [nil]",
        cleanup: Cleanup::Inlined { writes: false },
        inlined_write: false,
    },
    Shape {
        label: "stored receiver",
        ensure: "r0 := [^1]. r0 ensure: [{F}]",
        cleanup: Cleanup::Closure,
        inlined_write: false,
    },
    Shape {
        label: "stored cleanup",
        ensure: "c0 := [{F}]. [^1] ensure: c0",
        cleanup: Cleanup::Closure,
        inlined_write: false,
    },
    Shape {
        label: "stored receiver and cleanup",
        ensure: "r0 := [^1]. c0 := [{F}]. r0 ensure: c0",
        cleanup: Cleanup::Closure,
        inlined_write: false,
    },
    Shape {
        label: "stored cleanup, not writing",
        ensure: "c0 := [nil]. [^1] ensure: c0",
        cleanup: Cleanup::Closure,
        inlined_write: false,
    },
    Shape {
        label: "literal, writing through a self-send",
        ensure: "[^1] ensure: [self bump]",
        cleanup: Cleanup::Indirect,
        inlined_write: true,
    },
    Shape {
        label: "literal, running a stored block that writes",
        ensure: "c0 := [{F}]. [^1] ensure: [c0 value]",
        cleanup: Cleanup::Indirect,
        inlined_write: true,
    },
    Shape {
        label: "stored cleanup inside a literal one",
        ensure: "c0 := [{F}]. [[^1] ensure: c0] ensure: [{F}]",
        cleanup: Cleanup::Closure,
        inlined_write: true,
    },
];

/// The oracle: whether the cleanup can write the state a `^` of `scope`
/// carries, as far as the `ensure:` site can tell, so the catch needs a
/// relay. A block literal that writes nothing never does. A closure the site
/// cannot see into may write the actor-state cell: the state of an actor
/// closure or merged Tier 2 fun, and the cell an actor method's `^` stored
/// its `State` to before it threw. A value type's `Self` is written only by
/// an inlined cleanup (a closure cannot rebind it, and one that assigns a
/// field is rejected), and a class method's throw carries nothing (its class
/// variables are written in place). An indirect write (BT-4433) lands in the
/// actor-state cell, like a closure's: a self-send's callee and a stored
/// block both write the actor's state there; a value type's self-send
/// answers a new value and leaves the sender's `Self` as it was.
fn relay_needed(scope: Scope, cleanup: Cleanup) -> bool {
    match (scope, cleanup) {
        (_, Cleanup::Inlined { writes: false })
        | (Scope::ClassMethod, _)
        | (Scope::ValueTypeMethod, Cleanup::Closure | Cleanup::Indirect) => false,
        (_, Cleanup::Inlined { writes: true } | Cleanup::Closure | Cleanup::Indirect) => true,
    }
}

/// A value-type cell whose field write sits in a block compiled as a closure
/// (a stored block, or the cleanup of an `ensure:` whose receiver is one):
/// codegen rejects it, so it is no program and proves nothing.
fn rejected_value_type_closure(cell: &Cell, why: &str) -> bool {
    cell.scope == Scope::ValueTypeMethod && why.contains("Cannot assign to field 'k'")
}

/// ADR 0146 R1, the oracle: whether a foreign `^` crossing the cell's
/// `ensure:` must find the cleanup's writes stored to the actor-state cell.
/// That is where the cell is live and the cleanup is inlined and writes: an
/// actor method's threaded body. Not an actor closure or merged Tier 2 fun,
/// whose state already is the cell; not a value type or class method, which
/// have no actor state (a class method runs in its caller, ADR 0142). Who
/// throws plays no part: an `ensure:` of an actor method's threaded body
/// owes the clause whether or not its own scope also has a `^`, since a
/// foreign one may cross it either way.
fn foreign_store_needed(cell: &Cell) -> bool {
    cell.scope == Scope::ActorMethod && cell.inlined_write
}

struct Cell {
    scope: Scope,
    thrower: Thrower,
    label: String,
    stmt: String,
    cleanup: Cleanup,
    inlined_write: bool,
}

impl Cell {
    fn source(&self) -> String {
        let (header, _) = self.scope.class();
        format!(
            "{header}\n{}",
            self.thrower.methods(self.scope, "probe", &self.stmt)
        )
    }
}

fn cells() -> Vec<Cell> {
    let mut cells = Vec::new();
    for &scope in SCOPES {
        let (_, field) = scope.class();
        let write = format!("self.{field} := 9");
        for &thrower in THROWERS {
            for &(position, template) in POSITIONS {
                for shape in SHAPES {
                    let ensure = thrower.ensure(&shape.ensure.replace("{F}", &write));
                    cells.push(Cell {
                        scope,
                        thrower,
                        label: format!("{scope:?} x {thrower:?} x {position} x {}", shape.label),
                        stmt: template.replace("{E}", &ensure),
                        cleanup: shape.cleanup,
                        inlined_write: shape.inlined_write,
                    });
                }
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
        // Another method's `^` is no `^` of the `ensure:`'s scope: there is
        // nothing of its own to relay.
        let needed = cell.thrower == Thrower::OwnScope && relay_needed(cell.scope, cell.cleanup);
        if relays != needed {
            failures.push(format!(
                "{}: relays: {relays}, oracle: {needed}\n{}",
                cell.label,
                cell.source()
            ));
        }
        let stores = code.contains("_EnsureForeignNlr");
        if stores != foreign_store_needed(cell) {
            failures.push(format!(
                "{}: stores a foreign `^`'s cleanup: {stores}, oracle: {}\n{}",
                cell.label,
                foreign_store_needed(cell),
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
                .map(|(i, (c, _))| c.thrower.methods(scope, &format!("probe{i}"), &c.stmt))
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
    // BT-4433: every actor-scope cell whose cleanup writes only indirectly
    // compiles, so proofs 1 and 2 cover the heuristic its `writes_cell` reads.
    let indirect = compiled
        .iter()
        .filter(|(c, _)| c.cleanup == Cleanup::Indirect && relay_needed(c.scope, c.cleanup))
        .count();
    let indirect_shapes = SHAPES
        .iter()
        .filter(|shape| shape.cleanup == Cleanup::Indirect)
        .count();
    assert_eq!(
        indirect,
        3 * THROWERS.len() * POSITIONS.len() * indirect_shapes,
        "an indirect-write actor cell does not compile"
    );
    assert!(
        compiled.len() >= SCOPES.len() * THROWERS.len() * POSITIONS.len() * SHAPES.len() * 4 / 5,
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
        let expected = cell.thrower == Thrower::OwnScope && relay_needed(cell.scope, cell.cleanup);
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
        let needed = cell.thrower == Thrower::OwnScope && relay_needed(cell.scope, cell.cleanup);
        let agrees = match cell.scope {
            _ if cell.thrower == Thrower::OtherMethod => report.is_none(),
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

/// Proof 4 (ADR 0146 R1): with no foreign clause (every `ensure:` before
/// the fix), a cell fails `EnsureForeignCleanupNotStored` exactly when the
/// oracle says a foreign `^` crossing its `ensure:` must find the cleanup's
/// writes in the cell, and every foreign-throw cell in an actor method's
/// threaded body whose cleanup writes is one of them.
#[test]
fn ensure_relay_corpus_fails_when_a_foreign_return_takes_the_error_path() {
    let (compiled, mut failures) = compiled_cells();
    let mut fired_foreign = 0;
    for (cell, _) in &compiled {
        let src = cell.source();
        let report = with_fault(RelayFault::ForeignOnErrorPath, || compile(&src)).err();
        let lost = report
            .as_deref()
            .is_some_and(|r| r.contains("EnsureForeignCleanupNotStored") && r.contains("NoClause"));
        let expected = foreign_store_needed(cell);
        if cell.thrower == Thrower::OtherMethod {
            fired_foreign += usize::from(lost);
        }
        if lost != expected || (!expected && report.is_some()) {
            failures.push(format!(
                "{}: with no foreign clause, expected EnsureForeignCleanupNotStored: {expected}, got {report:?}",
                cell.label
            ));
        }
    }
    let foreign_writing = SHAPES.iter().filter(|shape| shape.inlined_write).count();
    assert_eq!(
        fired_foreign,
        POSITIONS.len() * foreign_writing,
        "not every foreign-throw actor-method cell whose cleanup writes fails"
    );
    assert!(
        failures.is_empty(),
        "{} cell(s) disagree:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

/// ADR 0146 R1 / ADR 0142: a class method that runs in its caller (its
/// class declares no class variables) has no actor `State`, even in an
/// actor class: its inlined `ensure:` emits no foreign clause and no store,
/// whoever throws. The same `ensure:` in an instance method of the class
/// does.
#[test]
fn a_class_method_running_in_its_caller_stores_no_foreign_cleanup() {
    let header = "Actor subclass: Bt4534CallerClassMethod\n  state: n = 0\n\n";
    let class_methods = "  class probe: blk =>\n    x := 0\n    [blk value] ensure: [x := 1]\n    x\n\n  class callProbe => self probe: [^1]\n";
    let instance_method = "  probe: blk =>\n    [blk value] ensure: [self.n := 40]\n    0\n";
    let class_only = format!("{header}{class_methods}");
    let code = compile(&class_only).expect("compiles with the verifier on");
    assert!(
        code.contains("primop 'raw_raise'"),
        "the class method has no `ensure:` catch:\n{code}"
    );
    assert!(
        !code.contains("_EnsureForeignNlr") && !code.contains("'beamtalk_actor_state':'store'"),
        "a class method running in its caller stores a foreign `^`'s cleanup:\n{code}"
    );
    // The verifier owes it nothing, so the pre-fix catch is no defect there.
    let report = with_fault(RelayFault::ForeignOnErrorPath, || compile(&class_only)).err();
    assert_eq!(report, None, "a class method's `ensure:` owes no store");
    // The control: the same `ensure:` in an instance method stores, and
    // fails without its foreign clause.
    let both = format!("{header}{class_methods}\n{instance_method}");
    let code = compile(&both).expect("compiles with the verifier on");
    assert!(
        code.contains("_EnsureForeignNlr") && code.contains("'beamtalk_actor_state':'store'"),
        "the instance method's `ensure:` has no foreign clause:\n{code}"
    );
    let report = with_fault(RelayFault::ForeignOnErrorPath, || compile(&both)).err();
    assert!(
        report
            .as_deref()
            .is_some_and(|r| r.contains("EnsureForeignCleanupNotStored")),
        "with no foreign clause, expected EnsureForeignCleanupNotStored, got {report:?}"
    );
    if erlc_available() {
        assert_compiles_through_erlc("test", &code);
    }
}

/// BT-4429: `Block>>ensure:` reached dynamically (`perform:`) compiles to the
/// structural fallback in the Block method, where every `^` is foreign. The
/// real stdlib `Block` relays every `$bt_nlr` throw with the cell over the
/// carried state when it was the cell's, through `erlc`; with the relay off
/// (the fallback before the fix) it fails `EnsureCleanupLostOnNlr`.
#[test]
fn block_ensure_fallback_relays_a_foreign_throw() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../stdlib/src/block.bt");
    let src = std::fs::read_to_string(path).expect("stdlib/src/block.bt");
    let code = compile(&src).expect("Block compiles with the verifier on");
    assert!(
        code.contains("call 'beamtalk_actor_state':'snapshot'()")
            && code.contains("call 'beamtalk_actor_state':'refresh_carried'("),
        "Block>>ensure: relays no foreign `^`:\n{code}"
    );
    if erlc_available() {
        assert_compiles_through_erlc("test", &code);
    }
    let report = with_fault(RelayFault::NoRelay, || compile(&src)).err();
    assert!(
        report
            .as_deref()
            .is_some_and(|r| r.contains("EnsureCleanupLostOnNlr") && r.contains("NoRelay")),
        "without the relay, expected EnsureCleanupLostOnNlr, got {report:?}"
    );
}

/// An actor method whose `ensure:` cleanup nests `depth - 1` more inlined
/// `ensure:`s, each in the cleanup of the one outside it, the innermost
/// cleanup writing `self.n := 41`; the `^` thrown by `thrower`.
fn nested_ensure_source(thrower: Thrower, depth: u32) -> String {
    let mut cleanup = "self.n := 41".to_string();
    for k in 2..=depth {
        cleanup = format!("[self.n := {k}] ensure: [{cleanup}]");
    }
    let stmt = thrower.ensure(&format!("[^1] ensure: [{cleanup}]"));
    let (header, _) = Scope::ActorMethod.class();
    format!(
        "{header}\n{}",
        thrower.methods(Scope::ActorMethod, "probe", &stmt)
    )
}

/// Proof 5 (BT-4598): the size of a nested `ensure:`. Each inlined `ensure:`
/// in an actor method's threaded body compiles its cleanup once per path that
/// runs it: the success path, the error path, and one `$bt_nlr` clause that
/// the scope's own throw (the relay) and a foreign one share. So a cleanup
/// nested `depth` deep is compiled exactly 3^depth times, whoever throws.
/// Before the relay and the foreign clause shared one body it was 4^depth
/// (at depth 3 the method was 43 KB of Core Erlang with 64 copies of the
/// innermost cleanup; it is 27 KB with 27); any regrowth fails here. Every depth
/// compiles with the verifier on and, at depth 3, through `erlc`.
#[test]
fn ensure_relay_corpus_nested_cleanup_grows_as_three_to_the_depth() {
    let mut failures = Vec::new();
    for &thrower in THROWERS {
        for depth in 1..=4 {
            let src = nested_ensure_source(thrower, depth);
            let code = match compile(&src) {
                Ok(code) => code,
                Err(why) => {
                    failures.push(format!("{thrower:?}, depth {depth}: {why}\n{src}"));
                    continue;
                }
            };
            let copies = code.matches(" = 41 in ").count();
            if copies != 3usize.pow(depth) {
                failures.push(format!(
                    "{thrower:?}, depth {depth}: the innermost cleanup is compiled {copies} times, expected {}\n{src}",
                    3usize.pow(depth)
                ));
            }
            let shared = code.contains("_EnsureNlrTok") && code.contains("_EnsureForeignNlr");
            if thrower == Thrower::OwnScope && !shared {
                failures.push(format!(
                    "{thrower:?}, depth {depth}: no relay sharing a foreign clause\n{code}"
                ));
            }
            if depth == 3
                && erlc_available()
                && catch_unwind(AssertUnwindSafe(|| {
                    assert_compiles_through_erlc("test", &code);
                }))
                .is_err()
            {
                failures.push(format!(
                    "{thrower:?}, depth {depth}: erlc rejected the module"
                ));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} nested `ensure:` cell(s) fail:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
