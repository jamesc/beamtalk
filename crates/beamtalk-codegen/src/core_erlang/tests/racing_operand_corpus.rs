// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4591: left-to-right operand evaluation is a checked invariant.
//!
//! `operand_order_check` scans every generated module (debug builds) for a list
//! cell or map literal holding two operands that run code. Here: the BT-4296
//! operand sources compile clean, and reintroducing the in-place list builder
//! (`LocalRebindFault::OperandsInPlace`) is reported. The second case reaches
//! the cell/loop-body actor `super` report, which no real program does.

use super::operand_evaluation_order::SRC;
use super::{codegen, verifier_report};
use crate::core_erlang::threaded_expr::fault::LocalRebindFault;

#[test]
fn sequenced_operand_lists_pass_the_check() {
    assert_eq!(verifier_report(SRC, None), None);
}

#[test]
fn a_reintroduced_in_place_list_builder_is_reported() {
    let report = verifier_report(SRC, Some(LocalRebindFault::OperandsInPlace))
        .expect("in-place operand lists must fail the check");
    assert!(report.contains("RacingOperands"), "{report}");
    for method in ["arrayOf:", "listOf:", "mapOf:"] {
        // Each construct is caught on its own, not only the first.
        let one = format!(
            "Value subclass: Probe\n  {}\n",
            match method {
                "arrayOf:" => "arrayOf: a => #[a bump, a n]",
                "listOf:" => "listOf: a => #(a bump, a n)",
                _ => "mapOf: a => #{#first => a bump, #second => a n}",
            }
        );
        assert_eq!(verifier_report(&one, None), None, "{method} clean");
        let report = verifier_report(&one, Some(LocalRebindFault::OperandsInPlace))
            .unwrap_or_else(|| panic!("{method} not reported"));
        assert!(report.contains("RacingOperands"), "{method}: {report}");
    }
}

/// An actor loop body sending `super` with a state-writing argument.
const CELL_SUPER: &str = "Actor subclass: Child
  state: n = 0
  bump => self.n := self.n + 1. self.n
  run => #(1, 2) do: [:_i | super pair: self bump with: 1]
";

#[test]
fn a_cell_super_send_with_a_state_argument_compiles_clean() {
    assert_eq!(verifier_report(CELL_SUPER, None), None);
    let _ = codegen(CELL_SUPER);
}

#[test]
fn skipping_super_argument_presequencing_reaches_the_opaque_escapes_report() {
    let report = verifier_report(CELL_SUPER, Some(LocalRebindFault::SuperArgsNotPresequenced))
        .expect("the escaping state bind must be reported");
    assert!(report.contains("StateEffectEscapesExpression"), "{report}");
}

/// BT-4591: the check found two in-place builders BT-4296 missed. An actor's
/// field defaults are the pairs of the `init/1` state map, so two defaults that
/// send messages ran last-first; a class variable read next to a send raced it.
#[test]
fn field_defaults_and_class_variable_reads_are_sequenced() {
    let defaults =
        "Actor subclass: Defaults\n  state: a = (Counter new)\n  state: b = (Counter new)\n";
    let code = super::codegen_named(defaults, "defaults");
    assert!(code.contains("_FieldDefault@"), "{code}");
    let class_var =
        "Actor subclass: ClassVar\n  classState: n = 0\n  class run: xs => #[xs last, self.n]\n";
    assert_eq!(verifier_report(class_var, None), None);
    let code = codegen(class_var);
    assert!(code.contains("_ArrLit@"), "{code}");
}
