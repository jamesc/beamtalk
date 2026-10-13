// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3802: a `State` rebind produced by a non-`self` cascade's operand must reach the
//! statement sequence that consumes the cascade.
//!
//! BT-3791 found a cascade lowered as an opaque value (`let _Result = <cascade> in
//! {'reply', _Result, State1}`) whose hoisted operand prelude bound `State1` inside that
//! value, so the rebind was scoped out of the reply and every later statement (`erlc`:
//! `unbound variable 'State1'`). The invariant is the ADR 0118 §Decision 5 one,
//! `VerifyError::StateEffectEscapesExpression`: a prelude carrying a versioned `Bind`
//! must be spliced into its consumer's frame, never rendered into a closed `Document`.
//! `generate_cascade` is the closed rendering of a cascade; it reports through
//! `report_threaded_ir_verify_errors` when its prelude carries such a `Bind`, so any
//! consumer that fails to splice a producing cascade (`threaded_expression`'s cascade
//! branch is the splice) fails the debug verifier in `codegen`, in the corpus below and
//! in `just verify-threaded-ir`.
//!
//! [`cascade_operand_corpus_threads_state`] proves it over cascade operand position x
//! consumer shape; each program must pass the debug verifier and compile through `erlc`.

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::threaded_ir::{
    BindOp, FrameId, ThreadedStmt, ValueRef, VerifyError, VersionPrefix, VersionedVar,
    opaque_escapes,
};
use beamtalk_core::source_analysis::Span;
use std::panic::{AssertUnwindSafe, catch_unwind};

/// The closed-cascade check reports exactly the versioned `Bind`s of a prelude, and
/// nothing for statements that bind no version.
#[test]
fn opaque_escapes_reports_version_binds_only() {
    let span = Span::new(1, 2);
    let prelude = vec![
        ThreadedStmt::Statement(beamtalk_cerl_doc::Document::Str("let _ = 1 in "), span),
        ThreadedStmt::Bind {
            target: VersionedVar::new(VersionPrefix::State, 1, FrameId::ROOT),
            source: VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT),
            op: BindOp::Direct(ValueRef::Literal("'_'")),
            span,
        },
    ];
    assert_eq!(
        opaque_escapes(&prelude, false),
        vec![VerifyError::StateEffectEscapesExpression {
            prefix: VersionPrefix::State,
            at: span,
        }]
    );
    assert!(opaque_escapes(&prelude[..1], false).is_empty());
}

const HEADER: &str = "Actor subclass: CascadeOperandProbe
  state: n = 0

  bump -> Integer =>
    self.n := self.n + 1
    self.n

  mk -> Object =>
    self.n := self.n + 1
    Object classBuilder

  take: x -> Integer => x
";

// (A field assignment as a cascade argument, `superclass: (self.n := 5)`, is deliberately
/// absent: `generate_field_assignment_open` produces no `Bind`, so its `State` rebind is
/// invisible to this check and trapped like BT-3803's send argument. BT-3803 owns it and
/// adds that shape to this corpus.)
///
/// A cascade (`CASC`) whose operand at the named position changes state. All are
/// non-`self` cascades; the `builder_*` ones are the `ClassBuilder` shape
/// (`classMethods:` map values and keys, `addClassMethod:body:`).
const CASCADES: &[(&str, &str)] = &[
    ("receiver", "(self mk) name: #A; superclass: Object"),
    (
        "first_arg",
        "Object classBuilder name: (self bump); superclass: Object",
    ),
    (
        "later_arg",
        "Object classBuilder name: #A; superclass: (self bump)",
    ),
    (
        "every_operand",
        "(self mk) name: (self bump); superclass: (self bump); register",
    ),
    (
        "builder_map_value",
        "Object classBuilder name: #A; superclass: Object; classMethods: #{#k => (self bump)}; addClassMethod: #j body: self.n; register",
    ),
    (
        "builder_map_key",
        "Object classBuilder name: #A; superclass: Object; classMethods: #{(self bump) => [:self | 1], #m => 2}; addClassMethod: #j body: self.n; register",
    ),
    (
        "builder_map_key_and_value_order",
        "Object classBuilder name: #A; superclass: Object; classMethods: #{(self bump) => (self bump), #z => [:self | 3], #m => (self bump)}; register",
    ),
    (
        "builder_literal_block_and_computed",
        "Object classBuilder name: #A; superclass: Object; classMethods: #{#k => (self bump), #answer => [:self | self k]}; addClassMethod: #j body: self.n; register",
    ),
];

/// A consumer of the cascade `CASC`, in a method whose later statements read state.
const CONSUMERS: &[(&str, &str)] = &[
    ("statement_then_read", "    CASC.\n    self.n\n"),
    ("last_expression", "    CASC\n"),
    ("assigned_then_read", "    c := CASC.\n    c.\n    self.n\n"),
    (
        "after_field_write",
        "    self.n := self.n + 1.\n    CASC.\n    self.n := self.n + 1.\n    self.n\n",
    ),
    (
        "argument_of_self_send",
        "    self take: (CASC).\n    self.n\n",
    ),
    ("receiver_of_send", "    (CASC) printString.\n    self.n\n"),
    (
        "conditional_arm",
        "    self.n > 5 ifTrue: [CASC] ifFalse: [self bump].\n    self.n\n",
    ),
    (
        "loop_body",
        "    i := 0.\n    [i < 2] whileTrue: [CASC. i := i + 1].\n    self.n\n",
    ),
];

#[test]
fn cascade_operand_corpus_threads_state() {
    let passes = |src: &str| {
        catch_unwind(AssertUnwindSafe(|| {
            assert_compiles_through_erlc("test", &codegen(src));
        }))
        .is_ok()
    };
    let mut failures: Vec<String> = Vec::new();
    for (consumer_name, consumer) in CONSUMERS {
        let cases: Vec<(&str, String)> = CASCADES
            .iter()
            .enumerate()
            .map(|(i, (cascade_name, cascade))| {
                (
                    *cascade_name,
                    format!("  probe{i} =>\n{}", consumer.replace("CASC", cascade)),
                )
            })
            .collect();
        // One `erlc` run per consumer over every cascade at once...
        let methods: Vec<&str> = cases.iter().map(|(_, m)| m.as_str()).collect();
        if passes(&format!("{HEADER}\n{}", methods.join("\n"))) {
            continue;
        }
        // ...and one per case only when that fails, so a failure names its case.
        for (cascade_name, method) in &cases {
            let single = format!("{HEADER}\n{method}");
            if !passes(&single) {
                failures.push(format!("{consumer_name} / {cascade_name}:\n{single}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} cascade case(s) fail verified codegen:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}
