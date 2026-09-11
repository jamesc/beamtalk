// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Value-type `self`/field-write threading through loop
//! constructs (`to:do:`, `timesRepeat:`, `do:`, `collect:`,
//! `select:`, `inject:into:`, and nested loop families) and
//! conditionals.

use super::*;

// ─── value-type `Self` threading through loops and conditionals ────
//
// `CodeGenContext::ValueType` reaches `self.field := value` only via the
// documented, permanent `TestCase` exemption (see
// `check_value_slot_assignment` in `beamtalk-core`'s `class_validators.rs`) —
// a genuine `Value subclass:` rejects the write outright (ADR 0042). So every
// fixture below is a `TestCase subclass:`, matching the only shape a user can
// actually write. The runtime ground truths for these exact shapes are pinned
// in `stdlib/test/value_type_mutation_matrix_test.bt`'s axis-4 cells.

#[test]
fn test_value_type_field_write_in_to_do_threads_self_through_tail_call() {
    // Before this fix, the loop's `letrec 'loop'/2 = fun (_loopidx, StateAcc)`
    // carried only the outer-locals map: the body's own
    // `let Self1 = maps:put('total', _Val, Self) in` was correct but never
    // reached the recursive `apply`, so every iteration discarded it and the
    // method's trailing `self.total` read the ORIGINAL `Self` parameter.
    // Compiled fine; returned 0 instead of 15.
    //
    // Also serves as a guard-rail fixture: this is the top-level-statement
    // shape that `reject_unthreadable_value_self_field_write` must let
    // through, so an over-firing rejection surfaces here as a failed
    // `expect` below.
    let src = concat!(
        "TestCase subclass: VtLoopSelfThread\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    seen := 0\n",
        "    1 to: 5 do: [:i |\n",
        "      self.total := self.total + i\n",
        "      seen := seen + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtloopselfthread").with_workspace_mode(true),
    )
    .expect("value-type field write inside to:do: must compile");

    assert!(
        code.contains("fun (_loopidx3, StateAcc, Self)"),
        "the loop's letrec fun must take `Self` as an extra trailing parameter. Got:\n{code}"
    );
    assert!(
        code.contains("apply 'loop'/3 (call 'erlang':'+'(_loopidx3, 1), StateAcc1, Self1)"),
        "the recursive tail call must carry the body's own mutated Self1 forward. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', StateAcc, Self}"),
        "the exit arm must return the fun's own incoming Self in the trailing slot. Got:\n{code}"
    );
    assert!(
        code.contains("let Self1 = call 'erlang':'element'(3,"),
        "the post-loop rebind must extract the threaded Self from tuple slot 3. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the method's trailing field read must see the post-loop Self1, not the \
         original Self parameter. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtloopselfthread", &code);
}

#[test]
fn test_value_type_field_only_loop_packs_from_fresh_map_not_state() {
    // A second manifestation of the same bug: a loop whose ONLY mutation is
    // the field write has no threaded locals at all, so `generate_pack_prefix`
    // short-circuited to the ambient `initial_state_var` — the actor `State`,
    // which does not exist in a value-type method. `erlc` rejected the result
    // with "unbound variable 'State'".
    //
    // Also serves as a second guard-rail fixture: the same top-level write
    // with NO sibling local to thread, which must likewise survive
    // `reject_unthreadable_value_self_field_write`'s root-node skip.
    let src = concat!(
        "TestCase subclass: VtLoopSelfOnly\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    1 to: 5 do: [:i | self.total := self.total + i]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtloopselfonly").with_workspace_mode(true),
    )
    .expect("a field-write-only value-type loop must compile");

    assert!(
        code.contains("call 'maps':'new'()"),
        "a value-type loop must pack its accumulator from a fresh map. Got:\n{code}"
    );
    assert!(
        !code.contains("(_temp1, State)"),
        "the initial apply must never pass the (nonexistent) actor State. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtloopselfonly", &code);
}

#[test]
fn test_value_type_field_write_in_if_true_merges_self_out_of_branch() {
    // The true arm must not bind `Self1` only inside its own nested `let`
    // while the code AFTER the `case` references it unconditionally — the
    // branch-merge tuple needs a trailing `Self` slot both arms carry.
    let src = concat!(
        "TestCase subclass: VtCondSelfThread\n",
        "  field: total = 0\n\n",
        "  computeTotal: flag =>\n",
        "    seen := 0\n",
        "    flag ifTrue: [\n",
        "      self.total := self.total + 7\n",
        "      seen := seen + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtcondselfthread").with_workspace_mode(true),
    )
    .expect("value-type field write inside ifTrue: must compile");

    assert!(
        code.contains("{Seen, Self1}"),
        "the taken arm must return its own mutated Self in the merge tuple. Got:\n{code}"
    );
    assert!(
        code.contains("{Seen, Self}"),
        "the untaken arm must pass the pre-case Self through the same slot. Got:\n{code}"
    );
    assert!(
        code.contains("let Self1 = call 'erlang':'element'(2,"),
        "the post-case rebind must extract the merged Self. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the trailing field read must see the merged Self1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtcondselfthread", &code);
}

#[test]
fn test_value_type_field_write_in_if_true_if_false_merges_both_arms() {
    let src = concat!(
        "TestCase subclass: VtCondBothSelfThread\n",
        "  field: total = 0\n\n",
        "  computeTotal: flag =>\n",
        "    flag\n",
        "      ifTrue: [self.total := self.total + 7]\n",
        "      ifFalse: [self.total := self.total + 100]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtcondbothselfthread").with_workspace_mode(true),
    )
    .expect("value-type field writes in both conditional arms must compile");

    // No outer-local mutation at all here: the merge tuple's only slot is
    // `Self`, so each arm returns a 1-tuple and the rebind reads element 1.
    assert!(
        code.contains("let Self1 = call 'erlang':'element'(1,"),
        "with no threaded locals the merged Self is the tuple's only slot. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('total', Self1)"),
        "the trailing field read must see the merged Self1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@vtcondbothselfthread", &code);
}

#[test]
fn test_value_type_field_write_in_nested_loop_is_compile_error() {
    // This construct's accepted scope limit mirrors the identical one for
    // class vars (`ClassVarMutationLostAcrossNestedLoop`): the inner loop
    // threads its own `Self` correctly, but nothing unpacks a nested loop's
    // trailing `Self` slot back into the OUTER loop's statement sequence, so
    // the mutation would be silently discarded. Rejected cleanly instead.
    let src = concat!(
        "TestCase subclass: VtNestedLoopSelf\n",
        "  field: total = 0\n\n",
        "  computeTotal =>\n",
        "    seen := 0\n",
        "    1 to: 3 do: [:i |\n",
        "      1 to: 3 do: [:j | self.total := self.total + j]\n",
        "      seen := seen + 1\n",
        "    ]\n",
        "    self.total\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtnestedloopself").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ValueSelfMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "field 'self.total'");
        }
        other => panic!(
            "Expected ValueSelfMutationLostAcrossNestedLoop for a value-type field write \
             inside a loop nested inside another loop. Got: {other:?}"
        ),
    }
}

#[test]
fn test_value_type_field_write_in_last_position_conditional_still_rejected() {
    // This construct's deliberate boundary: the LAST-position conditional path
    // (`emit_vt_conditional_case_to_var`) and the assignment-RHS path
    // (`emit_vt_conditional_assign_rhs`) thread outer LOCALS only, never
    // `Self` — routing a field write into either would silently drop it
    // (last position) or emit an `erlc`-rejected reference to a branch-scoped
    // `Self{N}` (assign RHS). Both are strictly worse than a clean
    // diagnostic, so
    // `is_conditional_with_vt_self_field_threading` (and hence
    // `VtBodyExprKind::ConditionalWithSelfFieldThreading`) is scoped to the
    // non-last statement position only, and these shapes keep erroring.
    let src = concat!(
        "TestCase subclass: VtCondSelfLast\n",
        "  field: total = 0\n\n",
        "  computeTotal: flag =>\n",
        "    flag ifTrue: [self.total := self.total + 7]\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@vtcondselflast").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "a value-type field write in a LAST-position conditional stays a clean compile \
         error, not a silent drop. Got: {result:?}"
    );
}

#[test]
fn test_class_var_write_in_conditional_nested_in_loop_is_compile_error() {
    // The reference half of the parity pair: a class-var write
    // inside an `ifTrue:` inside a `to:do:` has ALWAYS been rejected cleanly.
    // `needs_mutation_threading`'s `in_class_method()` arm does not count
    // field writes, so the branch block never reaches the inline
    // mutation-threading path and falls through to `generate_block`'s
    // `validate_stored_closure` diagnostic. Pinned here so the value-type
    // half below is measured against real, executed behaviour rather than a
    // remembered claim.
    let field = field_assignment_rejection_field(
        concat!(
            "Object subclass: CvCondInLoop\n",
            "  classState: total = 0\n\n",
            "  class computeTotal: flag =>\n",
            "    seen := 0\n",
            "    1 to: 3 do: [:i |\n",
            "      flag ifTrue: [self.total := self.total + i]\n",
            "      seen := seen + 1\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@cvcondinloop",
    );
    assert_eq!(field, "total");
}

#[test]
fn test_value_type_field_write_in_conditional_nested_in_loop_is_compile_error() {
    // The half this fixes — byte-for-byte the class-var fragment above,
    // with `classState:`/`class ` swapped for the `TestCase` value-type
    // exemption.
    //
    // The write is not a TOP-LEVEL statement of the loop body, so
    // `loop_body_threads_value_self` (and hence
    // `ThreadingPlan::threads_value_self`) reports `false` and the loop
    // threads no `Self`: the conditional's own `Self{N}` rebind stays scoped
    // to its nested `let`. Before this issue that meant a silently dropped
    // mutation here, and — with the field write as the loop body's ONLY
    // mutation, see the sibling test below — an outright `erlc` crash.
    // `reject_unthreadable_value_self_field_write` now rejects it with the same
    // diagnostic the class-var half above already got.
    let field = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtCondInLoopSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal: flag =>\n",
            "    seen := 0\n",
            "    1 to: 3 do: [:i |\n",
            "      flag ifTrue: [self.total := self.total + i]\n",
            "      seen := seen + 1\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtcondinloopself",
    );
    assert_eq!(field, "total");
}

#[test]
fn test_value_type_field_write_in_conditional_nested_in_loop_without_sibling_local_is_compile_error()
 {
    // The headline repro: the same shape with NO sibling local
    // mutation, so the loop has no threaded locals at all. Before this fix
    // `generate_pack_prefix` short-circuited to the ambient actor `State` —
    // a variable a value-type method does not have — and `erlc` rejected the
    // whole module with "unbound variable 'State'", the crash that made this
    // shape worse than its silently-dropping sibling above rather than merely
    // equal to it.
    let field = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtCondInLoopOnly\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    1 to: 5 do: [:i |\n",
            "      i > 2 ifTrue: [self.total := self.total + i]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtcondinlooponly",
    );
    assert_eq!(field, "total");
}

#[test]
fn test_value_type_field_write_in_foldl_body_is_compile_error() {
    // A `Foldl*` (`do:`/`collect:`/…) accumulator has no trailing
    // `Self` slot, so `ThreadingPlan::threads_value_self` is never set for a
    // fold plan and EVERY value-type field write in a fold body is
    // unthreadable — top-level statement included, unlike the `Letrec` case.
    // Both shapes crashed `erlc` with "unbound variable 'State'" before this
    // fix; both are now the same clean diagnostic the class-var equivalents
    // already produced.
    let nested = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlCondSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3) do: [:i |\n",
            "      i > 2 ifTrue: [self.total := self.total + i]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlcondself",
    );
    assert_eq!(nested, "total");

    let top_level = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlTopSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3) do: [:i | self.total := self.total + i]\n",
            "    self.total\n",
        ),
        "bt@vtfoldltopself",
    );
    assert_eq!(top_level, "total");

    // `collect:` too, so the "EVERY `Foldl*` selector" claim above is backed
    // by a second member of the family rather than by `do:` alone — the two
    // share `lower_foldl_body`, and this pins that they also share its
    // rejection.
    let collect = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlCollectSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3) collect: [:i | self.total := self.total + i]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlcollectself",
    );
    assert_eq!(collect, "total");
}

#[test]
fn test_value_type_field_write_in_remaining_foldl_selectors_is_compile_error() {
    // The headline repro (`do:`) and `collect:` are pinned above;
    // this test rounds out the rest of the `Foldl*` family —
    // `select:`/`reject:`/`inject:into:`/`detect:ifNone:`/`count:`,
    // plus a `Range`'s own `do:` (`(1 to: n) do: [...]`, which reaches the
    // exact same `generate_list_do_with_mutations` path as a list receiver's
    // `do:` — the dispatch is keyed on the `do:` selector and block body,
    // never on the receiver's type). All six share `lower_foldl_body` with
    // `do:`/`collect:`, so — per `reject_unthreadable_value_self_field_write`'s
    // doc comment — `threads_value_self` is `false` for every one of them and
    // the same clean `FieldAssignmentInUnsupportedBlock` diagnostic already
    // produced for `do:`/`collect:` fires here too, rather than the
    // `erlc` "unbound variable 'State'" crash the pre-fix parent commit
    // produced for this whole family.
    let select = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlSelectSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3) select: [:i | self.total := self.total + i. i > 1]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlselectself",
    );
    assert_eq!(select, "total");

    let reject = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlRejectSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3) reject: [:i | self.total := self.total + i. i > 1]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlrejectself",
    );
    assert_eq!(reject, "total");

    let inject_into = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlInjectSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3)\n",
            "      inject: 0\n",
            "      into: [:acc :i |\n",
            "        self.total := self.total + i\n",
            "        acc + i\n",
            "      ]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlinjectself",
    );
    assert_eq!(inject_into, "total");

    let detect_if_none = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlDetectSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3)\n",
            "      detect: [:i | self.total := self.total + i. i > 10]\n",
            "      ifNone: [-1]\n",
            "    self.total\n",
        ),
        "bt@vtfoldldetectself",
    );
    assert_eq!(detect_if_none, "total");

    let count = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlCountSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    #(1, 2, 3) count: [:i | self.total := self.total + i. i > 1]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlcountself",
    );
    assert_eq!(count, "total");

    let range_do = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtFoldlRangeDoSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    (1 to: 3) do: [:i | self.total := self.total + i]\n",
            "    self.total\n",
        ),
        "bt@vtfoldlrangedoself",
    );
    assert_eq!(range_do, "total");
}

#[test]
fn test_value_type_field_write_nested_in_other_loop_families_and_constructs_is_compile_error() {
    // The headline repro is a `to:do:`, but the gap was never
    // specific to that selector — every loop family that reaches
    // `lower_letrec_body`/`lower_foldl_body` carried it. Each fixture below
    // was verified BROKEN on the parent commit (d2bbdc9) before being pinned
    // here, so none of these is a shape the rejection newly takes away:
    //
    // * `whileTrue:` — the worst of the set: it COMPILED and silently
    //   returned 0 instead of 12, the "silently dropped" half of the
    //   bug class rather than a crash.
    // * `timesRepeat:`, a loop nested in a loop, and `inject:into:` — all
    //   three crashed `erlc` with "unbound variable 'State'".
    let while_true = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtWhileCondSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    i := 0\n",
            "    [i < 5] whileTrue: [\n",
            "      i := i + 1\n",
            "      i > 2 ifTrue: [self.total := self.total + i]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtwhilecondself",
    );
    assert_eq!(while_true, "total");

    let times_repeat = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtTimesRepeatCondSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal: flag =>\n",
            "    3 timesRepeat: [\n",
            "      flag ifTrue: [self.total := self.total + 1]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vttimesrepeatcondself",
    );
    assert_eq!(times_repeat, "total");

    let nested_loop = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtNestedLoopCondSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    1 to: 3 do: [:i |\n",
            "      1 to: 2 do: [:j |\n",
            "        j > 1 ifTrue: [self.total := self.total + 1]\n",
            "      ]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtnestedloopcondself",
    );
    assert_eq!(nested_loop, "total");

    let inject_into = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtInjectCondSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    r := #(1, 2, 3) inject: 0 into: [:acc :i |\n",
            "      i > 1 ifTrue: [self.total := self.total + i]\n",
            "      acc + i\n",
            "    ]\n",
            "    r + self.total\n",
        ),
        "bt@vtinjectcondself",
    );
    assert_eq!(inject_into, "total");

    // Not a loop family but the same gap on a different axis: the nested
    // construct hiding the write is an `ensure:` handler rather than a
    // conditional. It lowers through its own (exception) path, so it would
    // not be covered by any amount of `ifTrue:` fixtures above — and it too
    // crashed `erlc` with "unbound variable 'State'" on the parent commit.
    let ensure_block = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtEnsureInLoopSelf\n",
            "  field: total = 0\n\n",
            "  computeTotal =>\n",
            "    1 to: 3 do: [:i |\n",
            "      [i] ensure: [self.total := self.total + i]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtensureinloopself",
    );
    assert_eq!(ensure_block, "total");
}

#[test]
fn test_value_type_nested_field_write_rejected_beside_threadable_top_level_write() {
    // The sharpest case, and the one that pins the rejection's
    // SELECTIVITY rather than merely its existence: one loop body holding
    // BOTH a bare top-level write (`total`, which `plan.threads_value_self`
    // does carry and the root-node skip must let through) AND a write nested
    // in a conditional (`other`, which nothing carries).
    //
    // The asserted field name is the discriminator. A rejection keyed off the
    // loop body as a whole — or one that forgot the root-node skip — would
    // name `total` here and still "produce the right error variant"; only a
    // per-statement walk that skips the threadable root reports `other`.
    //
    // On the parent commit this fixture did not merely miscompile: it
    // panicked the compiler outright (a `Result::expect` unwind out of
    // `run`), making it the most severe of the shapes this issue closes.
    let field = field_assignment_rejection_field(
        concat!(
            "TestCase subclass: VtMixedWriteSelf\n",
            "  field: total = 0\n",
            "  field: other = 0\n\n",
            "  computeTotal: flag =>\n",
            "    1 to: 3 do: [:i |\n",
            "      self.total := self.total + i\n",
            "      flag ifTrue: [self.other := 1]\n",
            "    ]\n",
            "    self.total\n",
        ),
        "bt@vtmixedwriteself",
    );
    assert_eq!(
        field, "other",
        "the rejection must name the NESTED write, not the threadable \
         top-level one beside it"
    );
}

#[test]
fn test_value_type_multiple_top_level_field_writes_in_loop_still_thread() {
    // A guard rail complementing the two fixtures named in the
    // comment below: those pin a SINGLE top-level write, so neither would
    // catch a rejection that fired once a loop body held more than one. Two
    // top-level writes are still fully threadable (verified running correctly
    // on the parent commit and unchanged here), so this must keep compiling
    // with a `Self`-threaded tail call.
    let src = concat!(
        "TestCase subclass: VtMultiTopWrite\n",
        "  field: total = 0\n",
        "  field: count = 0\n\n",
        "  computeTotal =>\n",
        "    1 to: 3 do: [:i |\n",
        "      self.total := self.total + i\n",
        "      self.count := self.count + 1\n",
        "    ]\n",
        "    self.total + self.count\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@vtmultitopwrite").with_workspace_mode(true),
    )
    .expect("two bare top-level value-type writes must still compile");
    assert_compiles_through_erlc("bt@vtmultitopwrite", &code);
}
