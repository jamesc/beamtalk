// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5339: a read of a `late` field may raise, so it is a racing operand.
//!
//! `last_racing_operand` treated every field read as stable: it reads a `State`
//! version, so no sibling can change what it answers. A `late` field read also
//! raises `uninitialized_state_error` while its slot is unassigned (ADR 0124
//! §3). Two of them in one list cell stayed in place, erlc evaluated the tail
//! first, and `#(self.a, self.b)` with both unassigned raised for `b` instead
//! of `a`. beamtalk-watcher's `Watcher>>configureRegistry` (an actor send whose
//! argument list held two `late` reads) failed the debug verifier with
//! `RacingOperands`. The runtime value is pinned by
//! `stdlib/test/late_field_operand_order_test.bt`; the generated grid is
//! `late_field_operand_order_corpus.rs`.

use super::{codegen_named, verifier_report};
use crate::core_erlang::fault::Fault;

/// The reduced beamtalk-watcher shape plus the plain list literal.
pub(super) const SRC: &str = "typed Actor subclass: LateOps
  late state: first :: Integer
  late state: second :: Integer
  state: count :: Integer = 0

  listOfBoth -> List => #(self.first, self.second)

  sendOfBoth: s :: Object => s pair: self.first with: self.second

  pair: a :: Object with: b :: Object -> Object => a

  selfSendOfBoth => self pair: self.first with: self.second

  eagerAndLate -> List => #(self.count, self.first)
";

#[test]
fn two_late_reads_in_one_operand_list_pass_the_check() {
    assert_eq!(verifier_report(SRC, None), None);
}

#[test]
fn late_reads_left_in_place_are_reported() {
    let report = verifier_report(SRC, Some(Fault::LateFieldReadsStable))
        .expect("two in-place late reads in a list cell must fail the check");
    assert!(report.contains("RacingOperands"), "{report}");
}

/// The first `late` read is bound to a `_ListLit` temp ahead of the list, so
/// it is evaluated (and raises) first; the last racing operand stays in place.
#[test]
fn the_first_late_read_is_bound_before_the_list() {
    let code = codegen_named(SRC, "late_ops");
    assert!(
        code.contains("let _ListLit@")
            && code.contains("= case call 'maps':'find'('first', State)"),
        "the first late read must be bound to a temp:\n{code}"
    );
    assert!(
        !code.contains("[case call 'maps':'find'('first', "),
        "the first late read must not stay inside the list cell:\n{code}"
    );
    assert!(
        code.contains(", case call 'maps':'find'('second', State)"),
        "the last late read stays in place in the list cell:\n{code}"
    );
}

/// An eager field read cannot raise: with one `late` read beside it nothing
/// races, and the list stays in place with no temp.
#[test]
fn an_eager_read_beside_one_late_read_stays_in_place() {
    let code = codegen_named(SRC, "late_ops");
    assert!(
        code.contains("[call 'maps':'get'('count', "),
        "an eager read and a single late read do not race:\n{code}"
    );
}

/// A self-send's argument list is built in place (`capture_argument_list_doc`),
/// so `sequence_children` must hoist every racing child, not only `late`
/// reads: two divisions that may raise, and a `late` read nested in a compound
/// argument, evaluate in source order.
#[test]
fn self_send_arguments_that_may_raise_are_sequenced() {
    for body in [
        "m => self pair: self.count / 0 with: self.count / 1",
        "m => self pair: self.first + 1 with: self.second + 1",
        "m => self pair: self.count / 0 with: self.first + 1",
    ] {
        let src = format!(
            "typed Actor subclass: SelfArgs\n  late state: first :: Integer\n  late state: second :: Integer\n  state: count :: Integer = 0\n\n  pair: a :: Object with: b :: Object -> Object => a\n\n  {body}\n"
        );
        assert_eq!(verifier_report(&src, None), None, "{body}");
        // The pre-BT-5339 `sequence_children` rule leaves them in place.
        let report = verifier_report(&src, Some(Fault::SequenceChildrenBlockCapturesOnly))
            .unwrap_or_else(|| panic!("pre-fix sequence_children rule not reported: {body}"));
        assert!(report.contains("RacingOperands"), "{body}: {report}");
    }
}
