// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0111 Phase C `TupleAcc` unpack invariants: flat
//! unpack-mode-mismatch, early-exit accumulator liveness, the
//! `select_tuple_acc`/`select_direct_params` structural exclusions, and the
//! `lower_and_render` shim for `TupleAccUnpack` rendering.

use super::*;

/// Test-facing convenience: builds the same fixture [`build_tuple_acc_unpack`]
/// does (always in genuine `TupleAcc` mode — production only ever calls
/// `generate_tuple_unpack_docs` from inside `if plan.use_tuple_acc { .. }`,
/// so a `StateAcc`-mode unpack fixture is a hand-built-IR-only scenario, see
/// `verify_tuple_acc_unpack_mode_mismatch_fires_outside_any_tuple_acc_context`
/// / the dedicated `StateAcc` variant below) and verifies it in one call.
/// Test-only (`#[cfg(test)]`): production calls
/// [`build_tuple_acc_unpack`] + [`verify`] directly (`generate_tuple_unpack_docs`,
/// `control_flow/mod.rs`) since it also needs the built `ThreadedStmt` for
/// [`render`], which this convenience wrapper discards.
fn verify_tuple_acc_unpack_invariant(
    mode_gate_slots: usize,
    node_gate_slots: usize,
    threaded_locals: &[String],
    span: Span,
) -> Vec<VerifyError> {
    let (stmt, _) = build_tuple_acc_unpack(
        "StateAcc",
        mode_gate_slots,
        node_gate_slots,
        threaded_locals,
        span,
    );
    verify(std::slice::from_ref(&stmt))
}

/// Invariant class 2: `select_tuple_acc`'s `ValueType`-context exclusion.
/// Compares `use_tuple_acc` against a direct re-check of the same `context`
/// value `select_tuple_acc` itself already guards on.
///
/// No production call site — `select_tuple_acc`'s own early return
/// on `matches!(context, CodeGenContext::ValueType)` already makes
/// `use_tuple_acc && context_is_value_type` unconditionally unreachable by
/// inspection of that one function (`control_flow/mod.rs`), the same
/// already-structural shape found for `ThreadingModeUnpackMismatch`
/// — see `threaded_ir`'s module docs §Status. Kept as a regression pin,
/// exercised directly by hand-built-IR unit tests below (mirrors
/// `ThreadingModeUnpackMismatch`'s own kept-but-uncalled precedent).
fn verify_tuple_acc_value_type_exclusion(
    use_tuple_acc: bool,
    context_is_value_type: bool,
    span: Span,
) -> Vec<VerifyError> {
    if use_tuple_acc && context_is_value_type {
        vec![VerifyError::TupleAccInValueTypeContext { at: span }]
    } else {
        Vec::new()
    }
}

/// Invariant class 3: the recursive inter-construct `StateAcc`-fallback
/// invariant `list_op_needs_stateacc_fallback_recursive` encodes. Compares
/// `direct_params_selected` against `BodyEffects::has_non_tuple_safe_list_op`
/// (an independently computed recursive scan of the loop body for nested
/// list ops that need a `StateAcc` fallback).
///
/// No production call site — `select_direct_params`'s own
/// `!effects.has_non_tuple_safe_list_op` conjunct already makes
/// `direct_params_selected && inner_needs_stateacc_fallback` unconditionally
/// unreachable by inspection of that one function (`control_flow/mod.rs`) —
/// see `verify_tuple_acc_value_type_exclusion`'s doc comment for the full
/// rationale, shared verbatim. Kept as a regression pin.
fn verify_nested_list_op_stateacc_compat(
    direct_params_selected: bool,
    inner_needs_stateacc_fallback: bool,
    span: Span,
) -> Vec<VerifyError> {
    if direct_params_selected && inner_needs_stateacc_fallback {
        vec![VerifyError::NestedStateAccFallbackUnderDirectParams { at: span }]
    } else {
        Vec::new()
    }
}

// ── ADR 0111 Phase C: invariant class 1 — flat positional-unpack
// accumulator ─────────────────────────────────────────────────────────

#[test]
fn verify_tuple_acc_unpack_invariant_silent_under_tuple_acc_mode() {
    // The expected, common case: TupleAcc mode with a matching gate_slots
    // unpack — this is what every real `generate_tuple_unpack_docs` call
    // site produces (`mode_gate_slots`/`node_gate_slots` agree).
    let errors = verify_tuple_acc_unpack_invariant(
        0, // ListOpKind::Do's declared gate_slots
        0, // do:'s index_offset(1) - 1
        &["sum".to_string(), "count".to_string()],
        span(),
    );
    assert_eq!(errors, Vec::new());
}

#[test]
fn verify_tuple_acc_unpack_mode_mismatch_fires_in_stateacc_fallback_context() {
    // A TupleAccUnpack node hand-placed inside a StateAcc-fallback body
    // (never legitimate in production — `generate_tuple_unpack_docs` is
    // only ever called from inside `if plan.use_tuple_acc { .. }` — but a
    // regression pin for the invariant `TupleAccUnpackModeMismatch`
    // exists to catch: `verify_tuple_acc_unpack_invariant` dropped its old
    // `use_tuple_acc`/`fallback_reason` params since production has no
    // such call site anymore, so this constructs the fixture directly,
    // mirroring `verify_tuple_acc_unpack_mode_mismatch_fires_outside_any_tuple_acc_context`'s
    // `DirectParams` sibling below.
    let frame = FrameId::new(1);
    let param = AccParam::new("StateAcc");
    let target = local("sum", 1, frame);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::TupleAccUnpack {
            param,
            gate_slots: 0,
            targets: vec![target.clone()],
            frame,
            span: span(),
        }],
        produces: vec![target],
        span: span(),
    }];
    let errors = verify(&ir);
    assert_eq!(
        errors,
        vec![VerifyError::TupleAccUnpackModeMismatch {
            mode: ThreadingMode::StateAcc(StateAccFallbackReason::SelfSendInBody),
            at: span(),
        }]
    );
}

#[test]
fn verify_tuple_acc_unpack_mode_mismatch_fires_outside_any_tuple_acc_context() {
    // A TupleAccUnpack node hand-placed inside a DirectParams body (never
    // legitimate — DirectParams has no accumulator tuple at all).
    let frame = FrameId::new(1);
    let param = AccParam::new("StateAcc");
    let target = local("sum", 1, frame);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::DirectParams,
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::TupleAccUnpack {
            param,
            gate_slots: 0,
            targets: vec![target.clone()],
            frame,
            span: span(),
        }],
        produces: vec![target],
        span: span(),
    }];
    let errors = verify(&ir);
    assert!(
        errors.iter().any(|e| matches!(
            e,
            VerifyError::TupleAccUnpackModeMismatch {
                mode: ThreadingMode::DirectParams,
                ..
            }
        )),
        "expected TupleAccUnpackModeMismatch, got: {errors:?}"
    );
}

// ── Invariant class 4 — early-exit accumulator liveness ───────────────

#[test]
fn verify_tuple_acc_unpack_invariant_silent_with_gate_slots_for_early_exit_op() {
    // detect:/takeWhile:/dropWhile:/partition:'s shape: 2 leading gate
    // slots (index_offset: 3) before the threaded locals.
    let errors = verify_tuple_acc_unpack_invariant(
        2, // ListOpKind::TwoSlot's declared gate_slots
        2, // index_offset(3) - 1
        &["item_var".to_string()],
        span(),
    );
    assert_eq!(errors, Vec::new());
}

#[test]
fn verify_tuple_acc_unpack_invariant_fires_when_mode_and_node_gate_slots_disagree() {
    // `mode_gate_slots`/`node_gate_slots` are two genuinely independent
    // arguments (not the same value threaded twice —
    // see `build_tuple_acc_unpack`'s doc comment) — a call site whose
    // declared `ListOpKind` disagrees with its own `index_offset` fires
    // for real, exercised here through the actual production helper
    // (not just a hand-built `ThreadedStmt`, `verify_early_exit_gate_slot_mismatch_fires_when_node_disagrees_with_mode`
    // below).
    let errors = verify_tuple_acc_unpack_invariant(
        1, // wrongly declared ListOpKind::Accumulate
        2, // but built with a detect:-shaped index_offset(3) - 1
        &["found".to_string()],
        span(),
    );
    assert_eq!(
        errors,
        vec![VerifyError::EarlyExitGateSlotMismatch {
            mode_gate_slots: 1,
            node_gate_slots: 2,
            at: span(),
        }]
    );
}

#[test]
fn verify_early_exit_gate_slot_mismatch_fires_when_node_disagrees_with_mode() {
    // Hand-built fixture: the enclosing TupleAcc mode resolved 1 gate slot
    // (collect:/select:-shaped), but the unpack node itself was built with
    // 2 (detect:-shaped) — e.g. a future refactor that copies one op's
    // codegen onto another without updating its index_offset. Silently
    // wrong values, not a core_lint failure — the exact danger class
    // EarlyExitGateSlotMismatch exists to catch.
    let frame = FrameId::new(1);
    let param = AccParam::new("AccSt0");
    let target = local("item_var", 1, frame);
    let ir = vec![ThreadedStmt::Threaded {
        mode: ThreadingMode::TupleAcc(1),
        frame,
        shadow_write_eligible: true,
        body: vec![ThreadedStmt::TupleAccUnpack {
            param,
            gate_slots: 2,
            targets: vec![target.clone()],
            frame,
            span: span(),
        }],
        produces: vec![target],
        span: span(),
    }];
    let errors = verify(&ir);
    assert_eq!(
        errors,
        vec![VerifyError::EarlyExitGateSlotMismatch {
            mode_gate_slots: 1,
            node_gate_slots: 2,
            at: span(),
        }]
    );
}

#[test]
fn verify_tuple_acc_unpack_invariant_distinguishes_gate_slot_shapes() {
    // do: (0 gates), collect:/select:-family (1 gate), and
    // detect:/takeWhile:-family (2 gates) are all independently silent
    // when the node's gate_slots matches its enclosing mode — pins that
    // the check is genuinely parameterized per op-family shape, not
    // hardcoded to one.
    for gate_slots in [0usize, 1, 2] {
        let errors =
            verify_tuple_acc_unpack_invariant(gate_slots, gate_slots, &["v".to_string()], span());
        assert_eq!(
            errors,
            Vec::new(),
            "gate_slots={gate_slots} should be silent, got: {errors:?}"
        );
    }
}

// ── Invariant class 2 — select_tuple_acc's ValueType-context exclusion ─

#[test]
fn verify_tuple_acc_value_type_exclusion_silent_when_tuple_acc_not_selected() {
    assert_eq!(
        verify_tuple_acc_value_type_exclusion(false, true, span()),
        Vec::new()
    );
}

#[test]
fn verify_tuple_acc_value_type_exclusion_silent_in_actor_context() {
    assert_eq!(
        verify_tuple_acc_value_type_exclusion(true, false, span()),
        Vec::new()
    );
}

#[test]
fn verify_tuple_acc_value_type_exclusion_fires_when_both_true() {
    // Simulates the regression `select_tuple_acc`'s own ValueType guard
    // prevents today: `use_tuple_acc: true` selected in a `ValueType`
    // context, which would reference an unbound `State`.
    let errors = verify_tuple_acc_value_type_exclusion(true, true, span());
    assert_eq!(
        errors,
        vec![VerifyError::TupleAccInValueTypeContext { at: span() }]
    );
}

// ── Invariant class 3 — recursive inter-construct StateAcc fallback ────

#[test]
fn verify_nested_list_op_stateacc_compat_silent_when_direct_params_not_selected() {
    assert_eq!(
        verify_nested_list_op_stateacc_compat(false, true, span()),
        Vec::new()
    );
}

#[test]
fn verify_nested_list_op_stateacc_compat_silent_when_no_nested_fallback() {
    assert_eq!(
        verify_nested_list_op_stateacc_compat(true, false, span()),
        Vec::new()
    );
}

#[test]
fn verify_nested_list_op_stateacc_compat_fires_when_both_true() {
    // Simulates the regression `select_direct_params`'s own
    // `!effects.has_non_tuple_safe_list_op` guard prevents today: a
    // DirectParams loop containing a nested list op whose inner block
    // needs a StateAcc fallback — DirectParams has no StateAcc map for
    // that inner `{value, StateAcc}` result to unpack into.
    let errors = verify_nested_list_op_stateacc_compat(true, true, span());
    assert_eq!(
        errors,
        vec![VerifyError::NestedStateAccFallbackUnderDirectParams { at: span() }]
    );
}

// ── lower_and_render (test shim) for TupleAccUnpack ────────────────────

#[test]
fn lower_and_render_tuple_acc_unpack_renders_element_chain() {
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::TupleAccUnpack {
        param: AccParam::new("StateAcc"),
        gate_slots: 0,
        targets: vec![local("sum", 1, frame), local("count", 1, frame)],
        frame,
        span: span(),
    }];
    let rendered = lower_and_render(&ir).to_pretty_string();
    assert_eq!(
        rendered,
        "let Sum1 = call 'erlang':'element'(1, StateAcc) in \
         let Count1 = call 'erlang':'element'(2, StateAcc) in "
    );
}

#[test]
fn lower_and_render_tuple_acc_unpack_respects_gate_slots_offset() {
    // detect:-shaped: 2 gate slots, so the first threaded local starts
    // at tuple position 3, not 1.
    let frame = FrameId::new(1);
    let ir = vec![ThreadedStmt::TupleAccUnpack {
        param: AccParam::new("AccSt0"),
        gate_slots: 2,
        targets: vec![local("n", 1, frame)],
        frame,
        span: span(),
    }];
    let rendered = lower_and_render(&ir).to_pretty_string();
    assert_eq!(rendered, "let N1 = call 'erlang':'element'(3, AccSt0) in ");
}
