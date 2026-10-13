// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0131 Phase 2 (BT-3749; BT-3748 review): each ADR 0131 §4 verifier
//! check fires on a real compile, not only on hand-built IR
//! (`threaded_ir/tests/local_rebind.rs`). The local-threading producer is the
//! first production source of `LocalRebind`s, so these compile one §1a probe
//! shape — `t + (<fold that writes t>)` — in each method context with one
//! deliberately broken lowering injected
//! ([`threaded_expr::fault`](crate::core_erlang::threaded_expr::fault)) and
//! assert the verifier reports it. The unbroken compile is the control.

use super::codegen;
use crate::core_erlang::threaded_expr::fault::{self, LocalRebindFault};

/// `t` is read as the left operand, then rebound by the right operand's
/// producer (ADR 0131 §1a).
const BODY: &str =
    "    t := 0\n    r := t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])\n    #[r, t]\n";

fn programs() -> [String; 3] {
    [
        format!("Object subclass: CorpusClass\n  class go =>\n{BODY}"),
        format!("Value subclass: CorpusValue\n  go =>\n{BODY}"),
        format!("Actor subclass: CorpusActor\n  state: n = 0\n\n  go =>\n{BODY}"),
    ]
}

/// The verifier's debug-build report for `src` compiled with `fault`
/// active: the message of the panic `report_threaded_ir_verify_errors`
/// raises, or `None` when the compile succeeds.
fn verifier_report(src: &str, fault: LocalRebindFault) -> Option<String> {
    let result = fault::with(fault, || {
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)))
    });
    result.err().map(|payload| {
        payload
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
            .unwrap_or_default()
    })
}

fn assert_reports(fault: LocalRebindFault, error: &str) {
    for src in programs() {
        let report = verifier_report(&src, fault)
            .unwrap_or_else(|| panic!("{fault:?} compiled without a verifier report:\n{src}"));
        assert!(
            report.contains(error),
            "{fault:?} should be reported as {error}, got: {report}\n{src}"
        );
    }
}

#[test]
fn the_unbroken_producer_compiles_and_snapshots_the_left_operand() {
    for src in programs() {
        let code = codegen(&src);
        assert!(code.contains("let _Tmp"), "{src}\n{code}");
        assert!(code.contains("'__local__t'"), "{src}\n{code}");
    }
}

#[test]
fn a_sibling_read_in_place_after_a_later_rebind_is_local_read_after_sibling_rebind() {
    assert_reports(
        LocalRebindFault::SkipSiblingSnapshot,
        "LocalReadAfterSiblingRebind",
    );
}

#[test]
fn a_construct_tuple_without_its_rebind_is_threaded_local_dropped() {
    assert_reports(LocalRebindFault::DropRebind, "ThreadedLocalDropped");
}

#[test]
fn a_map_put_rebind_at_the_method_root_is_local_rebind_mode_mismatch() {
    assert_reports(LocalRebindFault::WrongLowering, "LocalRebindModeMismatch");
}

#[test]
fn a_rebind_recording_another_frame_is_local_rebind_frame_mismatch() {
    assert_reports(LocalRebindFault::WrongFrame, "LocalRebindFrameMismatch");
}
