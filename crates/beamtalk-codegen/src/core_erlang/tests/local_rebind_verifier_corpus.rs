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

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2b (BT-3793): the same checks on producers nested in loop
// and fold bodies, whose `LocalRebind`s are lowered by the enclosing frame.
// ---------------------------------------------------------------------------

/// The same §1a probe shape as a statement of a loop or fold body, in each
/// frame mode a body can have: a counted loop (direct params), a `do:` fold
/// (tuple accumulator, or the actor's `StateAcc`) and a `whileTrue:` loop.
fn loop_programs() -> Vec<String> {
    let mut out = Vec::new();
    for frame in [
        "1 to: 2 do: [:i | {s}]",
        "#(1, 2) do: [:i | {s}]",
        "[u < 2] whileTrue: [u := u + 1. {s}]",
    ] {
        let stmt = "r := t + (#(1, 2) inject: 0 into: [:a :x | t := t + 1. a + x])";
        let body = format!(
            "    t := 0\n    u := 0\n    r := 0\n    {}\n    #[r, t]\n",
            frame.replace("{s}", stmt)
        );
        out.push(format!(
            "Object subclass: LoopCorpusClass\n  class go =>\n{body}"
        ));
        out.push(format!("Value subclass: LoopCorpusValue\n  go =>\n{body}"));
        // An actor fold frame (`do:`) does not lower a value-bearing site
        // through the producer (`is_actor_fold_value_site`: the carrier does
        // not fit its tuple accumulator), so this operand-position probe has
        // no producer to break there. Its statement-position producers are
        // covered by the agreement test.
        if frame.starts_with("#(1, 2) do:") {
            continue;
        }
        out.push(format!(
            "Actor subclass: LoopCorpusActor\n  state: n = 0\n\n  go =>\n{body}"
        ));
    }
    out
}

fn assert_loop_reports(fault: LocalRebindFault, error: &str) {
    for src in loop_programs() {
        let report = verifier_report(&src, fault)
            .unwrap_or_else(|| panic!("{fault:?} compiled without a verifier report:\n{src}"));
        assert!(
            report.contains(error),
            "{fault:?} should be reported as {error}, got: {report}\n{src}"
        );
    }
}

#[test]
fn the_unbroken_loop_body_producer_compiles() {
    for src in loop_programs() {
        let code = codegen(&src);
        assert!(code.contains("let _Tmp"), "{src}\n{code}");
    }
}

#[test]
fn a_loop_body_sibling_read_in_place_is_local_read_after_sibling_rebind() {
    assert_loop_reports(
        LocalRebindFault::SkipSiblingSnapshot,
        "LocalReadAfterSiblingRebind",
    );
}

#[test]
fn a_loop_body_construct_tuple_without_its_rebind_is_threaded_local_dropped() {
    assert_loop_reports(LocalRebindFault::DropRebind, "ThreadedLocalDropped");
}

#[test]
fn a_loop_body_rebind_recording_another_frame_is_local_rebind_frame_mismatch() {
    assert_loop_reports(LocalRebindFault::WrongFrame, "LocalRebindFrameMismatch");
}

#[test]
fn a_map_put_rebind_in_a_direct_params_loop_body_is_local_rebind_mode_mismatch() {
    // The counted-loop frame in a class method threads its locals as letrec
    // parameters, so a `MapPut` there threads nothing.
    let src = &loop_programs()[0];
    let report = verifier_report(src, LocalRebindFault::WrongLowering)
        .unwrap_or_else(|| panic!("compiled without a verifier report:\n{src}"));
    assert!(
        report.contains("LocalRebindModeMismatch"),
        "{report}\n{src}"
    );
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 3b (BT-3817): the same checks on `on:do:` / `ensure:`
// (and, since Phase 4, BT-3750, `Result tryDo:`) producers, at the method
// root and in the frames they open or sit in (a conditional arm, a loop body,
// the protected body of another exception).
// ---------------------------------------------------------------------------

/// The §1a probe shape with an exception as the right operand, at the method
/// root and as a statement of each frame kind.
fn exception_programs() -> Vec<String> {
    let mut out = Vec::new();
    for (frame, stmt) in [
        (
            "{s}",
            "r := t + ([t := t + 1. 5] on: Error do: [:e | t := 9. 0])",
        ),
        ("{s}", "r := t + ([t := t + 1. 5] ensure: [t := t + 10])"),
        (
            "(u < 3) ifTrue: [{s}]",
            "r := t + ([t := t + 1. 5] ensure: [nil])",
        ),
        (
            "1 to: 2 do: [:i | {s}]",
            "r := t + ([t := t + 1. 5] ensure: [nil])",
        ),
        (
            "[[1] ensure: [nil]. {s}] on: Error do: [:e | 0]",
            "r := t + ([t := t + 1. 5] on: Error do: [:e | 0])",
        ),
        // ADR 0131 Phase 4 (BT-3750): `Result tryDo:` is the same producer,
        // its catch boundary in the runtime, at the root and in each frame.
        ("{s}", "r := t + (Result tryDo: [t := t + 1. 5])"),
        (
            "(u < 3) ifTrue: [{s}]",
            "r := t + (Result tryDo: [t := t + 1. 5])",
        ),
        (
            "1 to: 2 do: [:i | {s}]",
            "r := t + (Result tryDo: [t := t + 1. 5])",
        ),
        (
            "[[1] ensure: [nil]. {s}] on: Error do: [:e | 0]",
            "r := t + (Result tryDo: [t := t + 1. 5])",
        ),
        (
            "Result tryDo: [{s}. 1]",
            "r := t + (Result tryDo: [t := t + 1. 5])",
        ),
    ] {
        let body = format!(
            "    t := 0\n    u := 0\n    r := 0\n    {}\n    #[r, t]\n",
            frame.replace("{s}", stmt)
        );
        out.push(format!(
            "Object subclass: ExcCorpusClass\n  class go =>\n{body}"
        ));
        out.push(format!("Value subclass: ExcCorpusValue\n  go =>\n{body}"));
        out.push(format!(
            "Actor subclass: ExcCorpusActor\n  state: n = 0\n\n  go =>\n{body}"
        ));
    }
    out
}

fn assert_exception_reports(fault: LocalRebindFault, error: &str) {
    for src in exception_programs() {
        let report = verifier_report(&src, fault)
            .unwrap_or_else(|| panic!("{fault:?} compiled without a verifier report:\n{src}"));
        assert!(
            report.contains(error),
            "{fault:?} should be reported as {error}, got: {report}\n{src}"
        );
    }
}

#[test]
fn the_unbroken_exception_producer_compiles_and_snapshots_the_left_operand() {
    for src in exception_programs() {
        let code = codegen(&src);
        assert!(code.contains("let _Tmp"), "{src}\n{code}");
    }
}

#[test]
fn an_exception_sibling_read_in_place_is_local_read_after_sibling_rebind() {
    assert_exception_reports(
        LocalRebindFault::SkipSiblingSnapshot,
        "LocalReadAfterSiblingRebind",
    );
}

#[test]
fn an_exception_construct_tuple_without_its_rebind_is_threaded_local_dropped() {
    assert_exception_reports(LocalRebindFault::DropRebind, "ThreadedLocalDropped");
}

#[test]
fn an_exception_rebind_recording_another_frame_is_local_rebind_frame_mismatch() {
    assert_exception_reports(LocalRebindFault::WrongFrame, "LocalRebindFrameMismatch");
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 2d (BT-3798): the same checks on `Dictionary at:ifAbsent:`
// producers, at the method root and in the frames they sit in (a conditional
// arm, a protected body, the handler of another lookup).
// ---------------------------------------------------------------------------

/// The §1a probe shape with a lookup as the right operand, at the method root
/// and as a statement of each frame kind.
fn lookup_programs() -> Vec<String> {
    let mut out = Vec::new();
    for (frame, stmt) in [
        (
            "{s}",
            "r := t + (#{#a => 1} at: #k ifAbsent: [t := t + 1. 5])",
        ),
        ("{s}", "r := t + (#{#a => 1} at: #k ifAbsent: [t := 9])"),
        (
            "(u < 3) ifTrue: [{s}]",
            "r := t + (#{#a => 1} at: #k ifAbsent: [t := t + 1. 5])",
        ),
        (
            "[{s}] on: Error do: [:e | 0]",
            "r := t + (#{#a => 1} at: #k ifAbsent: [t := t + 1. 5])",
        ),
        (
            "#{#a => 1} at: #j ifAbsent: [{s}. 0]",
            "r := t + (#{#a => 1} at: #k ifAbsent: [t := t + 1. 5])",
        ),
    ] {
        let body = format!(
            "    t := 0\n    u := 0\n    r := 0\n    {}\n    #[r, t]\n",
            frame.replace("{s}", stmt)
        );
        out.push(format!(
            "Object subclass: LookupCorpusClass\n  class go =>\n{body}"
        ));
        out.push(format!(
            "Value subclass: LookupCorpusValue\n  go =>\n{body}"
        ));
        out.push(format!(
            "Actor subclass: LookupCorpusActor\n  state: n = 0\n\n  go =>\n{body}"
        ));
    }
    out
}

fn assert_lookup_reports(fault: LocalRebindFault, error: &str) {
    for src in lookup_programs() {
        let report = verifier_report(&src, fault)
            .unwrap_or_else(|| panic!("{fault:?} compiled without a verifier report:\n{src}"));
        assert!(
            report.contains(error),
            "{fault:?} should be reported as {error}, got: {report}\n{src}"
        );
    }
}

#[test]
fn the_unbroken_lookup_producer_compiles_and_calls_the_guard() {
    for src in lookup_programs() {
        let code = codegen(&src);
        assert!(code.contains("let _Tmp"), "{src}\n{code}");
        assert!(
            code.contains("'beamtalk_map':'lookup_for_threading'"),
            "{src}\n{code}"
        );
    }
}

#[test]
fn a_lookup_sibling_read_in_place_is_local_read_after_sibling_rebind() {
    assert_lookup_reports(
        LocalRebindFault::SkipSiblingSnapshot,
        "LocalReadAfterSiblingRebind",
    );
}

#[test]
fn a_lookup_construct_tuple_without_its_rebind_is_threaded_local_dropped() {
    assert_lookup_reports(LocalRebindFault::DropRebind, "ThreadedLocalDropped");
}

#[test]
fn a_lookup_rebind_recording_another_frame_is_local_rebind_frame_mismatch() {
    assert_lookup_reports(LocalRebindFault::WrongFrame, "LocalRebindFrameMismatch");
}

// ---------------------------------------------------------------------------
// ADR 0131 Phase 3c (BT-3822): the same checks on conditional producers, at
// the method root and as a statement of every loop and fold body frame mode
// (a counted loop's letrec parameters, a `whileTrue:` loop, a `do:`/`collect:`
// /`inject:into:` fold's tuple accumulator or an actor's `StateAcc`), where
// the enclosing frame lowers the construct's `LocalRebind`s.
// ---------------------------------------------------------------------------

/// The §1a probe shape with a conditional as the right operand, at the method
/// root and as a statement of each loop or fold frame.
fn conditional_programs() -> Vec<String> {
    let operand = "r := t + ((u < 3) ifTrue: [t := t + 1. 5] ifFalse: [6])";
    frame_programs(&[
        ("{s}", operand),
        ("1 to: 2 do: [:i | {s}]", operand),
        ("#(1, 2) do: [:i | {s}]", operand),
        ("#(1, 2) collect: [:i | {s}. i]", operand),
        ("#(1, 2) inject: 0 into: [:a :i | {s}. a + i]", operand),
        ("[u < 2] whileTrue: [u := u + 1. {s}]", operand),
        ("#(1, 2) do: [:i | 1 to: 2 do: [:j | {s}]]", operand),
        // A statement-position conditional in each frame mode.
        ("1 to: 2 do: [:i | {s}]", "(u < 3) ifTrue: [t := t + 1]"),
        ("#(1, 2) do: [:i | {s}]", "(u < 3) ifTrue: [t := t + 1]"),
    ])
}

/// The class, value-type and actor program of each `(frame, statement)` case:
/// `statement` replaces `{s}` in `frame`, between a `t := 0` prelude and a
/// read of `t`.
fn frame_programs(cases: &[(&str, &str)]) -> Vec<String> {
    let mut out = Vec::new();
    for (frame, stmt) in cases {
        let body = format!(
            "    t := 0\n    u := 0\n    r := 0\n    {}\n    #[r, t]\n",
            frame.replace("{s}", stmt)
        );
        out.push(format!(
            "Object subclass: CondCorpusClass\n  class go =>\n{body}"
        ));
        out.push(format!("Value subclass: CondCorpusValue\n  go =>\n{body}"));
        // An actor fold frame does not lower a value-bearing site through the
        // producer (`is_actor_fold_value_site`), so the operand probe has no
        // producer to break there; Phase 0 rejects it (see
        // `local_producer_agreement_conditional.rs` for the statement forms).
        if frame.contains("#(1, 2)") && stmt.starts_with("r := ") {
            continue;
        }
        out.push(format!(
            "Actor subclass: CondCorpusActor\n  state: n = 0\n\n  go =>\n{body}"
        ));
    }
    out
}

fn assert_conditional_reports(fault: LocalRebindFault, error: &str) {
    assert_frame_reports(&conditional_programs(), fault, error);
}

fn assert_frame_reports(programs: &[String], fault: LocalRebindFault, error: &str) {
    for src in programs {
        let Some(report) = verifier_report(src, fault) else {
            // A statement-position conditional has no sibling read to
            // snapshot: the fault only applies to the operand probes.
            assert!(
                fault == LocalRebindFault::SkipSiblingSnapshot && !src.contains("r := t + "),
                "{fault:?} compiled without a verifier report:\n{src}"
            );
            continue;
        };
        assert!(
            report.contains(error),
            "{fault:?} should be reported as {error}, got: {report}\n{src}"
        );
    }
}

#[test]
fn the_unbroken_conditional_producer_compiles_in_every_frame() {
    for src in conditional_programs() {
        let code = codegen(&src);
        assert!(
            code.contains("'__local__t'") || code.contains("Tmp"),
            "{src}\n{code}"
        );
    }
}

#[test]
fn a_conditional_sibling_read_in_place_is_local_read_after_sibling_rebind() {
    assert_conditional_reports(
        LocalRebindFault::SkipSiblingSnapshot,
        "LocalReadAfterSiblingRebind",
    );
}

#[test]
fn a_conditional_construct_tuple_without_its_rebind_is_threaded_local_dropped() {
    assert_conditional_reports(LocalRebindFault::DropRebind, "ThreadedLocalDropped");
}

#[test]
fn a_conditional_rebind_recording_another_frame_is_local_rebind_frame_mismatch() {
    assert_conditional_reports(LocalRebindFault::WrongFrame, "LocalRebindFrameMismatch");
}

// ---------------------------------------------------------------------------
// BT-3841: an evaluated literal block (`[...] value`) is the degenerate
// conditional: its one arm is a `BranchArm` frame that always runs, and the
// enclosing frame lowers the construct's `LocalRebind`s.
// ---------------------------------------------------------------------------

/// The §1a probe shape with an evaluated block as the right operand, at the
/// method root and as a statement of each loop or fold frame, and the
/// statement form in an arm.
fn evaluated_block_programs() -> Vec<String> {
    let operand = "r := t + ([t := t + 1. 5] value)";
    frame_programs(&[
        ("{s}", operand),
        ("1 to: 2 do: [:i | {s}]", operand),
        ("#(1, 2) do: [:i | {s}]", operand),
        ("#(1, 2) collect: [:i | {s}. i]", operand),
        ("#(1, 2) inject: 0 into: [:a :i | {s}. a + i]", operand),
        ("[u < 2] whileTrue: [u := u + 1. {s}]", operand),
        ("{s}", "[t := t + 1] value"),
        ("1 to: 2 do: [:i | {s}]", "[t := t + 1] value"),
        ("#(1, 2) do: [:i | {s}]", "[t := t + 1] value"),
        ("(u < 3) ifTrue: [{s}]", "[t := t + 1] value"),
        ("[{s}] value", "[t := t + 1] value"),
    ])
}

#[test]
fn the_unbroken_evaluated_block_producer_compiles_in_every_frame() {
    for src in evaluated_block_programs() {
        let code = codegen(&src);
        assert!(
            code.contains("'__local__t'") || code.contains("Tmp"),
            "{src}\n{code}"
        );
    }
}

#[test]
fn an_evaluated_block_sibling_read_in_place_is_local_read_after_sibling_rebind() {
    assert_frame_reports(
        &evaluated_block_programs(),
        LocalRebindFault::SkipSiblingSnapshot,
        "LocalReadAfterSiblingRebind",
    );
}

#[test]
fn an_evaluated_block_construct_tuple_without_its_rebind_is_threaded_local_dropped() {
    assert_frame_reports(
        &evaluated_block_programs(),
        LocalRebindFault::DropRebind,
        "ThreadedLocalDropped",
    );
}

#[test]
fn an_evaluated_block_rebind_recording_another_frame_is_local_rebind_frame_mismatch() {
    assert_frame_reports(
        &evaluated_block_programs(),
        LocalRebindFault::WrongFrame,
        "LocalRebindFrameMismatch",
    );
}
